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
(require 'lyqi-indent)

(defvar lyqi:prefered-languages '(italiano nederlands))
(defvar lyqi:prefered-octave-mode 'absolute)

;;; TODO: function for detecting note language
;;; TODO: function for detecting use of \relative

(defun lyqi:select-next-language (&optional syntax)
  (interactive)
  (let* ((syntax (or syntax (lp:current-syntax)))
         (current-language (slot-value (lyqi:language syntax) 'name))
         (next-language (loop for langs on lyqi:prefered-languages
                              for lang = (first langs)
                              if (eql lang current-language)
                              return (or (cadr langs) (first lyqi:prefered-languages)))))
    (set-slot-value syntax 'language (lyqi:select-language next-language))
    (force-mode-line-update)
    (lp:parse-and-highlight-buffer)))

(defun lyqi:header-line-select-next-language (event)
  "Like `lyqi:select-next-language', but temporarily select EVENT's window."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (lyqi:select-next-language)))

(defun lyqi:toggle-relative-mode (&optional syntax)
  (interactive)
  (let ((syntax (or syntax (lp:current-syntax))))
    (set-slot-value syntax 'relative-mode
                    (not (slot-value syntax 'relative-mode))))
  (force-mode-line-update))

(defun lyqi:header-line-toggle-relative-mode (event)
  "Like `lyqi:toggle-relative-mode', but temporarily select EVENT's window."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (lyqi:toggle-relative-mode)))

(defun lyqi:toggle-quick-edit-mode (&optional syntax)
  (interactive)
  (let ((syntax (or syntax (lp:current-syntax))))
    (set-slot-value syntax 'quick-edit-mode
                    (not (slot-value syntax 'quick-edit-mode))))
  (force-mode-line-update))

(defun lyqi:header-line-toggle-quick-edit-mode (event)
  "Like `lyqi:toggle-quick-edit-mode', but temporarily select EVENT's window."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (lyqi:toggle-quick-edit-mode)))

(defun lyqi:header-line-lyqi-mode (event)
  "Like `lyqi-mode', but temporarily select EVENT's window."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (lyqi-mode)))

(defun lyqi:set-header-line-format ()
  (setq header-line-format
        '(" "
          (:eval (propertize (symbol-name (slot-value (lyqi:language (lp:current-syntax)) 'name))
                             'help-echo "select next language"
                             'mouse-face 'mode-line-highlight
                             'local-map '(keymap
                                          (header-line
                                           keymap (mouse-1 . lyqi:header-line-select-next-language)))))
          " | "
          (:eval (propertize (if (slot-value (lp:current-syntax) 'relative-mode)
                                 "relative"
                                 "absolute")
                             'help-echo "toggle octave mode"
                             'mouse-face 'mode-line-highlight
                             'local-map '(keymap
                                          (header-line
                                           keymap (mouse-1 . lyqi:header-line-toggle-relative-mode)))))
          " mode | "
          (:eval (propertize (if (slot-value (lp:current-syntax) 'quick-edit-mode)
                                 "quick insert"
                                 "normal")
                             'help-echo "toggle edit mode"
                             'mouse-face 'mode-line-highlight
                             'local-map '(keymap
                                          (header-line
                                           keymap (mouse-1 . lyqi:header-line-toggle-quick-edit-mode)))))
          " edition"
          (:eval (if after-change-functions "" " | "))
          (:eval (if after-change-functions
                     ""
                     (propertize "¡BUG!"
                                 'help-echo "re-run lyqi-mode"
                                 'mouse-face 'mode-line-highlight
                                 'local-map '(keymap (header-line
                                                      keymap (mouse-1 . lyqi:header-line-lyqi-mode)))))))))

(defun lyqi-mode ()
  "Major mode for editing LilyPond music files, with quick insertion."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'lyqi-mode)
  (setq mode-name "Lyqi")
  ;; indentation
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lyqi:indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'lyqi:indent-region)
  ;; before and after change function (for parse update)
  (make-local-variable 'before-change-functions)
  (make-local-variable 'after-change-functions)
  (pushnew 'lp:before-parse-update before-change-functions)
  (setq after-change-functions '(lp:parse-update))
  ;; buffer syntax
  (make-local-variable 'lp:*current-syntax*)
  (unless lp:*current-syntax*
    (setq lp:*current-syntax*
          (make-instance 'lyqi:lilypond-syntax
                         :language (lyqi:select-language (first lyqi:prefered-languages))
                         :relative-mode (eql lyqi:prefered-octave-mode 'relative))))
  (lp:parse-and-highlight-buffer)
  ;; header line shows info on lyqi mode
  ;; TODO: custom variable to turn off header line
  (lyqi:set-header-line-format))

(provide 'lyqi-mode)
