#(use-modules (ice-9 format)
              (ice-9 regex))

#(define (print-symbols symbols)
   (let print-names ((pos 4)
                     (rest (sort (map (lambda (thing)
                                        (if (symbol? thing)
                                            (symbol->string thing)
                                            thing))
                                      symbols)
                                 string<=?)))
     (if (not (null? rest))
         (let* ((name (car rest))
                (len (string-length name)))
           (if (> (+ pos len 1) 80)
               (begin
                 (format #t "~%    ")
                 (set! pos 4)))
           (format #t "~a" name)
           (if (not (null? (cdr rest)))
               (format #t " "))
           (print-names (+ pos len 1) (cdr rest))))))

#(define (print-word-list name symbols)
   (format #t "(defconst ~a~%  '(" name)
   (print-symbols symbols)
   (format #t "))~2%"))

#(with-output-to-file
  "lyqi-words.el"
  (lambda ()
    (print-word-list "lyqi:lilypond-keywords"
                     (map car (ly:lexer-keywords (ly:parser-lexer parser))))
    (print-word-list "lyqi:lilypond-music-variables"
                     (filter identity
                             (module-map (lambda (symbol variable)
                                           (and (ly:music? (variable-ref variable))
                                                symbol))
                                         (current-module))))
    (print-word-list "lyqi:lilypond-music-functions"
                     (filter identity
                             (module-map (lambda (symbol variable)
                                           (and (ly:music-function? (variable-ref variable))
                                                symbol))
                                         (current-module))))
    (print-word-list "lyqi:lilypond-markup-commands"
                     (hash-fold (lambda (function dummy functions)
                                  (cons (regexp-substitute
                                         #f (string-match "-markup"
                                                          (symbol->string (procedure-name function)))
                                         'pre "" 'post)
                                        functions))
                                '()
                                markup-functions-properties))
    (print-word-list "lyqi:lilypond-markup-list-commands"
                     (hash-fold (lambda (function dummy functions)
                                  (cons (procedure-name function) functions))
                                '()
                                markup-list-functions))
    (print-word-list "lyqi:scheme-lily-procedures"
                    (filter identity
                          (module-map (lambda (symbol variable)
                                        (if (procedure? (variable-ref variable))
                                            (let ((name (symbol->string symbol)))
                                              (if (string-match ".*-markup$" name)
                                                  #f
                                                  symbol))
                                            #f))
                                      (module-public-interface (resolve-module '(lily))))))
    (print-word-list "lyqi:scheme-lily-macros"
                     (filter identity
                             (module-map (lambda (symbol variable)
                                           (and (macro? (variable-ref variable))
                                                symbol))
                                      (module-public-interface (resolve-module '(lily))))))
    (print-word-list "lyqi:scheme-lily-variables"
                     (filter identity
                             (module-map (lambda (symbol variable)
                                           (and (not (or (macro? (variable-ref variable))
                                                         (procedure? (variable-ref variable))))
                                                symbol))
                                      (module-public-interface (resolve-module '(lily))))))
    (print-word-list "lyqi:scheme-guile-procedures"
                     (filter identity
                             (module-map (lambda (symbol variable)
                                           (and (procedure? (variable-ref variable))
                                                symbol))
                                      (module-public-interface (resolve-module '(guile))))))
    (print-word-list "lyqi:scheme-guile-macros"
                     (filter identity
                             (module-map (lambda (symbol variable)
                                           (and (macro? (variable-ref variable))
                                                symbol))
                                      (module-public-interface (resolve-module '(guile))))))
    (format #t "(provide 'lyqi-words)~%")))
