EL_SOURCES = header.el lp-base.el lyqi-custom.el lyqi-pitchnames.el \
             lyqi-syntax.el lyqi-midi.el lyqi-editing-commands.el \
             lyqi-indent.el lyqi-compile-commands.el lyqi-mode.el

lyqi.el: $(EL_SOURCES)
	cat $(EL_SOURCES) | \
	sed -e 's/(provide .lyqi-.*)//' -e 's/(require .lyqi-.*)//' \
	-e 's/(provide .lp-.*)//' -e 's/(require .lp-.*)//' > $@
	echo "(provide 'lyqi)" >> $@

