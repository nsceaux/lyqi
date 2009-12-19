;;; Part of lyqi, a major emacs mode derived from LilyPond-Mode,
;;; for quick note insertion while editing GNU LilyPond music scores.
;;; 
;;; (c) copyright 2009 Nicolas Sceaux <nicolas.sceaux@free.fr>
;;; See http://nicolas.sceaux.free.fr/lilypond/

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'lyqi-syntax)

;;;
;;; Fontification
;;;
(defface lyqi:note-face
  '((((class color) (background dark)) :foreground "blue")
    (((class color) (background light)) :foreground "blue"))
  "Face for notes."
  :group 'lyqi-faces)

(defface lyqi:rest-face
  '((((class color) (background dark)) :foreground "green")
    (((class color) (background light)) :foreground "green"))
  "Face for rests and skips."
  :group 'lyqi-faces)

(defface lyqi:duration-face
  '((((class color) (background dark)) :foreground "red")
    (((class color) (background light)) :foreground "red"))
  "Face for rests and skips."
  :group 'lyqi-faces)

(defgeneric lyqi:fontify (form)
  "Fontify a lexeme or form")

(defmethod lyqi:fontify ((this lyqi:parser-symbol))
  (let* ((start (marker-position (lyqi:marker this)))
         (end (+ start (lyqi:size this))))
    (when (> end start)
      (set-text-properties start end (or (lyqi:face this) ())))))

(defmethod lyqi:fontify ((this lyqi:form))
  (let ((children (slot-value this 'children)))
    (if children
        (mapcar 'lyqi:fontify children)
        (call-next-method))))

(defmethod lyqi:fontify ((this lyqi:verbatim-form))
  (let* ((start (marker-position (lyqi:marker this)))
         (end (+ start (lyqi:size this))))
    (set-text-properties start end '())))

(defgeneric lyqi:face (form)
  "The face of a form, used in fontification.")

(defmethod lyqi:face ((this lyqi:parser-symbol))
  nil)

(defmethod lyqi:face ((this lyqi:note-lexeme))
  '(face lyqi:note-face))

(defmethod lyqi:face ((this lyqi:rest-skip-etc-lexeme))
  '(face lyqi:rest-face))

(defmethod lyqi:face ((this lyqi:duration-lexeme))
  '(face lyqi:duration-face))

(defmethod lyqi:face ((this lyqi:keyword-form))
  '(face font-lock-keyword-face))

(defmethod lyqi:face ((this lyqi:line-comment-start-lexeme))
  '(face font-lock-comment-delimiter-face))

(defmethod lyqi:face ((this lyqi:line-comment-lexeme))
  '(face font-lock-comment-face))

(provide 'lyqi-fontify)
