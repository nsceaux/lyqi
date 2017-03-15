AC_INIT([lyqi-mode], [0.3])
AC_PATH_PROG([locate], [locate])

# we use awk in the Makefile
AC_PROG_AWK
# find the directory containing tex hyphenation patterns
AC_MSG_CHECKING([location of TeX hyphenation pattern files])
LYQI_CHECK_HYPHEN_PATTERNS_DIR()
AC_MSG_RESULT([$hyphenation_patterns_dir])

# where is lilypond?
LYQI_CHECK_LILYPOND

AC_SUBST(lilypond_version)
AC_CONFIG_FILES(hyphenate.el package_header.el Makefile)
AC_OUTPUT
