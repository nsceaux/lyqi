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

(defun lyqi-mode ()
  "Major mode for editing LilyPond music files, with quick insertion."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'lyqi-mode)
  (setq mode-name "Lyqi")
  ;; local variables
  (make-local-variable 'after-change-functions)
  (setq after-change-functions '(lp:parse-update))
  (make-local-variable 'lp:*syntax*)
  (setq lp:*current-syntax* (lyqi:make-lilypond-syntax 'italiano))
  (lp:parse-and-highlight-buffer))
