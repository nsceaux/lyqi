EL_SOURCES = header.el lp-base.el lyqi-custom.el lyqi-pitchnames.el	\
             lyqi-words.el lyqi-syntax.el lyqi-midi.el			\
             lyqi-editing-commands.el lyqi-indent.el			\
             lyqi-compile-commands.el lyqi-completion.el		\
             lyqi-help-index.el lyqi-help.el lyqi-mode.el

LILYPOND_ORG_DOC_URL = http://lilypond.org/doc/v2.13/Documentation/notation

lilypond-index.html:
	wget -O $@ $(LILYPOND_ORG_DOC_URL)/lilypond-index.html

lyqi-help-index.el: lilypond-index.html
	perl make-help-index.pl lilypond-index.html $(LILYPOND_ORG_DOC_URL) $@

lyqi-words.el:
	lilypond make-words.ly

lyqi.el: $(EL_SOURCES)
	cat $(EL_SOURCES) | \
	sed -e 's/(provide .lyqi-[^)]*)//' -e 's/(require .lyqi-[^)]*)//' \
	-e 's/(provide .lp-[^)]*)//' -e 's/(require .lp-[^)]*)//' > $@
	echo "(provide 'lyqi)" >> $@

EMACS ?= emacs

lyqi.elc: lyqi.el
	$(EMACS) -batch -u $$USER -f batch-byte-compile lyqi.el

osx: lyqi.elc
	cd EmacsPointAndClick && make all
	cd MidiScript && make all

clean:
	rm -f lilypond-index.html lyqi-help-index.el lyqi-words.el lyqi.el lyqi.elc

.PHONY: osx linux clean
