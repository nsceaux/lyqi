dnl find the directory containing tex hyphenation patterns
AC_DEFUN([LYQI_CHECK_HYPHEN_PATTERNS_DIR], [
ly_hyph_file=hyph-es.tex

ly_hyphen_patterns=$(test -n "$locate" && $locate -e /$ly_hyph_file)

if test -n "$ly_hyphen_patterns" && test "$(echo -n $ly_hyphen_patterns|wc -l)" -eq 0; then
   hyphenation_patterns_dir=`AS_DIRNAME([$ly_hyphen_patterns])`
fi

# try standard locations
if test -z "$ly_hyphen_patterns"; then
  ly_hyphen_patterns=$(find /usr/share/tex* -type f -name $ly_hyph_file)

  if test -n "$ly_hyphen_patterns" && test "$(echo -n $ly_hyphen_patterns|wc -l)" -eq 0; then
    hyphenation_patterns_dir=`AS_DIRNAME([$ly_hyphen_patterns])`
  fi
fi
hyphenation_patterns_dir=\"$hyphenation_patterns_dir\"
AC_SUBST(hyphenation_patterns_dir)
])

AC_DEFUN([LYQI_CHECK_LILYPOND], [[
lilypond_version=$(lilypond --version|head -n1|sed -re 's/[^[:digit:]]*([[:digit:]]\.[[:digit:]]+).*/\1/')
]])
