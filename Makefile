EL_SOURCES = header.el lp-base.el lyqi-custom.el lyqi-pitchnames.el \
             lyqi-syntax.el lyqi-midi.el lyqi-editing-commands.el \
             lyqi-indent.el lyqi-mode.el

EIEIO_DIR = eieio

COMPILE_SCRIPT = compile-script

lyqi.el: $(EL_SOURCES)
	cat $(EL_SOURCES) | \
	sed -e 's/(provide .lyqi-.*)//' -e 's/(require .lyqi-.*)//' \
	-e 's/(provide .lp-.*)//' -e 's/(require .lp-.*)//' > $@
	echo "(provide 'lyqi)" >> $@

$(COMPILE_SCRIPT):
	@echo "(add-to-list 'load-path nil)" > $@
	@echo "(add-to-list 'load-path \"$(EIEIO_DIR)\")" >> $@
	@echo "(setq debug-on-error t)" >> $@

%.elc: %.el $(COMPILE_SCRIPT)
	emacs -batch --no-site-file -l $(COMPILE_SCRIPT) -f batch-byte-compile $<
