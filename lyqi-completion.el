;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; auto completion
;;;

(defmacro lyqi:define-completion-source (name category tip)
  (let* ((name (symbol-name name))
         (words-sym (intern (format "lyqi:%s" name)))
         (words-cache-sym (intern (format "lyqi:ac-%s" name)))
         (words-source-sym (intern (format "lyqi:ac-source-%s" name))))
    `(progn
       (defvar ,words-cache-sym nil
         ,(format "Auto completion: cache for %s words" tip))
       (defvar ,words-source-sym
         '((init . (or ,words-cache-sym
                       (setq ,words-cache-sym
                             (mapcar 'symbol-name ,words-sym))))
           (candidates . ,words-cache-sym)
           (symbol . ,(format "[%s]" tip))
           (prefix . ,(case category
                        ((lilypond-keyword) "\\\\\\(\\(?:\\sw\\|\\s_\\)*\\)")
                        ((scheme-function) "[#(]\\(\\(?:\\sw\\|\\s_\\)+\\)")
                        ((scheme-variable) "#\\(\\(?:\\sw\\|\\s_\\)+\\)")
                        (t nil)))
           (cache))))))

(defun lyqi:set-ac-sources (categories)
  (setq ac-sources
        (mapcar (lambda (sym)
                  (intern (format "lyqi:ac-source-%s" (symbol-name sym))))
                categories)))

(lyqi:define-completion-source lilypond-keywords lilypond-keyword "keyword")
(lyqi:define-completion-source lilypond-music-variables lilypond-keyword "variable")
(lyqi:define-completion-source lilypond-music-functions lilypond-keyword "function")
(lyqi:define-completion-source lilypond-markup-commands lilypond-keyword "markup")
(lyqi:define-completion-source lilypond-markup-list-commands lilypond-keyword "markup-list")
(lyqi:define-completion-source scheme-lily-procedures scheme-function "function")
(lyqi:define-completion-source scheme-lily-macros scheme-function "macro")
(lyqi:define-completion-source scheme-lily-variables scheme-variable "variable")
(lyqi:define-completion-source scheme-guile-procedures scheme-function "function")
(lyqi:define-completion-source scheme-guile-macros scheme-function "macro")

(defun lyqi:use-auto-complete ()
  (require 'auto-complete)
  (require 'lyqi-words)
  (lyqi:set-ac-sources '(lilypond-keywords
                         lilypond-music-variables
                         lilypond-music-functions
                         lilypond-markup-commands
                         lilypond-markup-list-commands
                         scheme-lily-procedures
                         scheme-lily-macros
                         scheme-lily-variables
                         scheme-guile-procedures
                         scheme-guile-macros))
  (setq ac-auto-start nil)
  (setq ac-dwim nil)
  (ac-set-trigger-key "TAB")
  (auto-complete-mode t))

(provide 'lyqi-completion)
