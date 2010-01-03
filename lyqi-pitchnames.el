;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LilyPond pitchnames
;;;

(defconst lyqi:+italian-pitchnames+
  '(("dobb" 0 -4) ("dobsb" 0 -3) ("dob" 0 -2) ("dosb" 0 -1)
    ("do" 0 0) ("dosd" 0 1) ("dod" 0 2) ("dodsd" 0 3) ("dodd" 0 4)
    ("rebb" 1 -4) ("rebsb" 1 -3) ("reb" 1 -2) ("resb" 1 -1)
    ("re" 1 0) ("resd" 1 1) ("red" 1 2) ("redsd" 1 3) ("redd" 1 4)
    ("mibb" 2 -4) ("mibsb" 2 -3) ("mib" 2 -2) ("misb" 2 -1)
    ("mi" 2 0) ("misd" 2 1) ("mid" 2 2) ("midsd" 2 3) ("midd" 2 4)
    ("fabb" 3 -4) ("fabsb" 3 -3) ("fab" 3 -2) ("fasb" 3 -1)
    ("fa" 3 0) ("fasd" 3 1) ("fad" 3 2) ("fadsd" 3 3) ("fadd" 3 4)
    ("solbb" 4 -4) ("solbsb" 4 -3) ("solb" 4 -2) ("solsb" 4 -1)
    ("sol" 4 0) ("solsd" 4 1) ("sold" 4 2) ("soldsd" 4 3) ("soldd" 4 4)
    ("labb" 5 -4) ("labsb" 5 -3) ("lab" 5 -2) ("lasb" 5 -1)
    ("la" 5 0) ("lasd" 5 1) ("lad" 5 2) ("ladsd" 5 3) ("ladd" 5 4)
    ("sibb" 6 -4) ("sibsb" 6 -3) ("sib" 6 -2) ("sisb" 6 -1)
    ("si" 6 0) ("sisd" 6 1) ("sid" 6 2) ("sidsd" 6 3) ("sidd" 6 4)))

(defconst lyqi:+dutch-pitchnames+
  '(("ceses" 0 -4) ("ceseh" 0 -3) ("ces" 0 -2) ("ceh" 0 -1)
    ("c" 0 0) ("cih" 0 1) ("cis" 0 2) ("cisih" 0 3) ("cisis" 0 4)
    ("deses" 1 -4) ("deseh" 1 -3) ("des" 1 -2) ("deh" 1 -1)
    ("d" 1 0) ("dih" 1 1) ("dis" 1 2) ("disih" 1 3) ("disis" 1 4)
    ("eeses" 2 -4) ("eses" 2 -4) ("eeseh" 2 -3) ("ees" 2 -2) ("es" 2 -2) ("eeh" 2 -1)
    ("e" 2 0) ("eih" 2 1) ("eis" 2 2) ("eisih" 2 3) ("eisis" 2 4)
    ("feses" 3 -4) ("feseh" 3 -3) ("fes" 3 -2) ("feh" 3 -1)
    ("f" 3 0) ("fih" 3 1) ("fis" 3 2) ("fisih" 3 3) ("fisis" 3 4)
    ("geses" 4 -4) ("geseh" 4 -3) ("ges" 4 -2) ("geh" 4 -1)
    ("g" 4 0) ("gih" 4 1) ("gis" 4 2) ("gisih" 4 3) ("gisis" 4 4)
    ("aeses" 5 -4) ("ases" 5 -4) ("aeseh" 5 -3) ("aes" 5 -2) ("as" 5 -2) ("aeh" 5 -1)
    ("a" 5 0) ("aih" 5 1) ("ais" 5 2) ("aisih" 5 3) ("aisis" 5 4)
    ("beses" 6 -4) ("beseh" 6 -3) ("bes" 6 -2) ("beh" 6 -1)
    ("b" 6 0) ("bih" 6 1) ("bis" 6 2) ("bisih" 6 3) ("bisis" 6 4)))

(defconst lyqi:+english-pitchnames+
  '(("cflatflat" 0 -4) ("cff" 0 -4) ("ctqf" 0 -3) ("cflat" 0 -2) ("cf" 0 -2) ("cqf" 0 -1) ("c" 0 0)
    ("cqs" 0 1) ("csharp" 0 2) ("cs" 0 2) ("ctqs" 0 3) ("css" 0 4) ("cx" 0 4)  ("csharpsharp" 0 4)
    ("dflatflat" 1 -4) ("dff" 1 -4) ("dtqf" 1 -3) ("df" 1 -2) ("dflat" 1 -2) ("dqf" 1 -1) ("d" 1 0)
    ("dqs" 1 1) ("ds" 1 2) ("dsharp" 1 2) ("dtqs" 1 3) ("dss" 1 4) ("dx" 1 4) ("dsharpsharp" 1 4)
    ("eflatflat" 2 -4) ("eff" 2 -4) ("etqf" 2 -3) ("ef" 2 -2) ("eflat" 2 -2) ("eqf" 2 -1) ("e" 2 0)
    ("eqs" 2 1) ("es" 2 2) ("esharp" 2 2) ("etqs" 2 3) ("ess" 2 4) ("ex" 2 4) ("esharpsharp" 2 4)
    ("fflatflat" 3 -4) ("fff" 3 -4) ("ftqf" 3 -3) ("ff" 3 -2) ("fflat" 3 -2) ("fqf" 3 -1) ("f" 3 0)
    ("fqs" 3 1) ("fs" 3 2) ("fsharp" 3 2) ("ftqs" 3 3) ("fss" 3 4) ("fx" 3 4) ("fsharpsharp" 3 4)
    ("gflatflat" 4 -4) ("gff" 4 -4) ("gtqf" 4 -3) ("gf" 4 -2) ("gflat" 4 -2) ("gqf" 4 -1) ("g" 4 0)
    ("gqs" 4 1) ("gs" 4 2) ("gsharp" 4 2) ("gtqs" 4 3) ("gss" 4 4) ("gx" 4 4) ("gsharpsharp" 4 4)
    ("aflatflat" 5 -4) ("aff" 5 -4) ("atqf" 5 -3) ("af" 5 -2) ("aflat" 5 -2) ("aqf" 5 -1) ("a" 5 0)
    ("aqs" 5 1) ("as" 5 2) ("asharp" 5 2) ("atqs" 5 3) ("ass" 5 4) ("ax" 5 4) ("asharpsharp" 5 4)
    ("bflatflat" 6 -4) ("bff" 6 -4) ("btqf" 6 -3) ("bf" 6 -2) ("bflat" 6 -2) ("bqf" 6 -1) ("b" 6 0)
    ("bqs" 6 1) ("bs" 6 2) ("bsharp" 6 2) ("btqs" 6 3) ("bss" 6 4) ("bx" 6 4) ("bsharpsharp" 6 4)))

(defconst lyqi:+german-pitchnames+
  '(("ceses" 0 -4) ("ceseh" 0 -3) ("ces" 0 -2) ("ceh" 0 -1)
    ("c" 0 0) ("cih" 0 1) ("cis" 0 2) ("cisih" 0 3) ("cisis" 0 4)
    ("deses" 1 -4) ("deseh" 1 -3) ("des" 1 -2) ("deh" 1 -1) ("d" 1 0)
    ("dih" 1 1) ("dis" 1 2) ("disih" 1 3) ("disis" 1 4)
    ("eses" 2 -4) ("eseh" 2 -3) ("es" 2 -2) ("eeh" 2 -1) ("e" 2 0)
    ("eih" 2 1) ("eis" 2 2) ("eisih" 2 3) ("eisis" 2 4)
    ("feses" 3 -4) ("feseh" 3 -3) ("fes" 3 -2) ("feh" 3 -1) ("f" 3 0)
    ("fih" 3 1) ("fis" 3 2) ("fisih" 3 3) ("fisis" 3 4)
    ("geses" 4 -4) ("geseh" 4 -3) ("ges" 4 -2) ("geh" 4 -1) ("g" 4 0)
    ("gih" 4 1) ("gis" 4 2) ("gisih" 4 3) ("gisis" 4 4)
    ("asas" 5 -4) ("ases" 5 -4) ("asah" 5 -3) ("aseh" 5 -3) ("as" 5 -2) ("aeh" 5 -1)
    ("a" 5 0) ("aih" 5 1) ("ais" 5 2) ("aisih" 5 3) ("aisis" 5 4)
    ("heses" 6 -4) ("heseh" 6 -3) ("b" 6 -2) ("beh" 6 -1)
    ("h" 6 0) ("hih" 6 1) ("his" 6 2) ("hisih" 6 3) ("hisis" 6 4)))

;;;
;;;
;;;
(defclass lyqi:language-data ()
  ((name                  :initarg :name)
   (name->pitch           :initarg :name->pitch)
   (pitch->name           :initarg :pitch->name)
   (pitch-regex           :initarg :pitch-regex)
   (octave-regex          :initarg :octave-regex)
   (note-regex            :initarg :note-regex)
   (rest-skip-regex       :initarg :rest-skip-regex)
   (duration-data         :initarg :duration-data)
   (duration-length-regex :initarg :duration-length-regex)
   (duration-regex        :initarg :duration-regex)))

(defmethod lyqi:pitchname ((this lyqi:language-data) pitch alteration)
  (let ((pitchnum (+ (* 10 pitch) alteration 4)))
    (cdr (assoc pitchnum (slot-value this 'pitch->name)))))

(defun lyqi:join (join-string strings)
  "Returns a concatenation of all strings elements, with join-string between elements"
  (apply 'concat 
	 (car strings) 
	 (mapcar (lambda (str) (concat join-string str))
		 (cdr strings))))

(defun lyqi:sort-string-by-length (string-list)
  "Sort the given string list by decreasing string length."
  (nreverse 
   (sort string-list
	 (lambda (str1 str2)
	   (or (< (length str1) (length str2))
	       (and (= (length str1) (length str2))
		    (string< str1 str2)))))))

(defun lyqi:make-language-data (name pitchnames)
  (let* ((pitch-regex (format "\\(%s\\)" 
                              (lyqi:join "\\|" (lyqi:sort-string-by-length
                                                (mapcar 'car pitchnames)))))
         (octave-regex "\\('+\\|,+\\)")
         (note-regex (format "%s%s?\\([^a-zA-Z]\\|$\\)" pitch-regex octave-regex))
         (rest-skip-regex "\\(r\\|R\\|s\\|q\\|\\\\skip\\)\\([^a-zA-Z]\\|$\\)")
         (duration-data '(("4" . 2)
                          ("8" . 3)
                          ("32" . 5)
                          ("64" . 6)
                          ("128" . 7) 
                          ("16" . 4)
                          ("256" . 8)
                          ("2" . 1)
                          ("1" . 0)
                          ("\\breve" . -1)
                          ("\\longa" . -2)
                          ("\\maxima" . -3)))
         (duration-length-regex
          (format "\\(%s\\)"
                  (lyqi:join "\\|"
                             (mapcar 'regexp-quote
                                     (lyqi:sort-string-by-length
                                      (mapcar 'car duration-data))))))
         (duration-regex (format "%s\\.*\\(\\*[0-9]+\\(/[0-9]+\\)?\\)?"
                                 duration-length-regex)))
    (make-instance 'lyqi:language-data
                   :name name
                   :name->pitch pitchnames
                   :pitch->name (loop for (name pitch alt) in pitchnames
                                       collect (cons (+ (* 10 pitch) alt 4) name))
                   :pitch-regex            pitch-regex
                   :octave-regex           octave-regex
                   :note-regex             note-regex
                   :rest-skip-regex        rest-skip-regex
                   :duration-data          duration-data
                   :duration-length-regex  duration-length-regex
                   :duration-regex         duration-regex)))

(defconst lyqi:+italian-language+ (lyqi:make-language-data 'italiano   lyqi:+italian-pitchnames+))
(defconst lyqi:+dutch-language+   (lyqi:make-language-data 'nederlands lyqi:+dutch-pitchnames+))
(defconst lyqi:+german-language+  (lyqi:make-language-data 'deutsch    lyqi:+german-pitchnames+))
(defconst lyqi:+english-language+ (lyqi:make-language-data 'english    lyqi:+english-pitchnames+))

(defun lyqi:select-language (language-name)
  (case language-name
    ((italiano) lyqi:+italian-language+)
    ((english)  lyqi:+english-language+)
    ((deutsch)  lyqi:+german-language+)
    (t          lyqi:+dutch-language+)))

(provide 'lyqi-pitchnames)
