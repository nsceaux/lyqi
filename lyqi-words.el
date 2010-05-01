(defconst lyqi:lilypond-keywords
  '(accepts addlyrics alias alternative book bookpart change chordmode chords 
    consists context default defaultchild denies description drummode drums 
    figuremode figures grobdescriptions header key layout lyricmode lyrics 
    lyricsto mark markup markuplines midi name new notemode objectid once 
    override paper partial relative remove repeat rest revert score sequential 
    set simultaneous skip tempo time times transpose type unset with))

(defconst lyqi:lilypond-music-variables
  '(accent aikenHeads arpeggio arpeggioArrowDown arpeggioArrowUp 
    arpeggioBracket arpeggioNormal arpeggioParenthesis 
    arpeggioParenthesisDashed autoBeamOff autoBeamOn balloonLengthOff 
    balloonLengthOn bassFigureExtendersOff bassFigureExtendersOn 
    bassFigureStaffAlignmentDown bassFigureStaffAlignmentNeutral 
    bassFigureStaffAlignmentUp bracketCloseSymbol bracketOpenSymbol break 
    cadenzaOff cadenzaOn coda compressFullBarRests cr cresc crescHairpin 
    crescTextCresc deadNotesOff deadNotesOn decr defaultTimeSignature dim 
    dimHairpin dimTextDecr dimTextDecresc dimTextDim dotsDown dotsNeutral 
    dotsUp downbow downmordent downprall dynamicDown dynamicNeutral dynamicUp 
    easyHeadsOff easyHeadsOn endcr endcresc enddecr enddim endincipit 
    episemFinis episemInitium escapedBiggerSymbol escapedExclamationSymbol 
    escapedParenthesisCloseSymbol escapedParenthesisOpenSymbol 
    escapedSmallerSymbol espressivo expandFullBarRests f fermata fermataMarkup 
    ff fff ffff fffff flageolet fp frenchChords fz germanChords glissando 
    halfopen harmonic harmonicsOff hideNotes hideSplitTiedTabNotes 
    hideStaffSwitch huge ignatzekExceptionMusic improvisationOff 
    improvisationOn italianChords laissezVibrer large lheel lineprall 
    longfermata ltoe marcato melisma melismaEnd mergeDifferentlyDottedOff 
    mergeDifferentlyDottedOn mergeDifferentlyHeadedOff mergeDifferentlyHeadedOn 
    mf mordent mp newSpacingSection noBeam noBreak normalsize 
    numericTimeSignature oneVoice open p palmMuteOff parenthesisCloseSymbol 
    parenthesisOpenSymbol partialJazzMusic phrasingSlurDashed 
    phrasingSlurDotted phrasingSlurDown phrasingSlurHalfDashed 
    phrasingSlurHalfSolid phrasingSlurNeutral phrasingSlurSolid phrasingSlurUp 
    pipeSymbol portato pp ppp pppp ppppp prall pralldown prallmordent 
    prallprall prallup predefinedFretboardsOff predefinedFretboardsOn repeatTie 
    reverseturn rfz rheel rtoe sacredHarpHeads segno semiGermanChords 
    setDefaultDurationToQuarter sf sff sfp sfz shiftOff shiftOn shiftOnn 
    shiftOnnn shortfermata showSplitTiedTabNotes showStaffSwitch 
    signumcongruentiae slurDashed slurDotted slurDown slurHalfDashed 
    slurHalfSolid slurNeutral slurSolid slurUp small snappizzicato sostenutoOff 
    sostenutoOn sp spp staccatissimo staccato startAcciaccaturaMusic 
    startAppoggiaturaMusic startGraceMusic startGroup startStaff startTextSpan 
    startTrillSpan stemDown stemNeutral stemUp stopAcciaccaturaMusic 
    stopAppoggiaturaMusic stopGraceMusic stopGroup stopStaff stopTextSpan 
    stopTrillSpan stopped sustainOff sustainOn tabFullNotation teeny tenuto 
    textLengthOff textLengthOn textSpannerDown textSpannerNeutral textSpannerUp 
    thumb tieDashed tieDotted tieDown tieHalfDashed tieHalfSolid tieNeutral 
    tieSolid tieUp tildeSymbol tiny treCorde trill tupletDown tupletNeutral 
    tupletUp turn unHideNotes unaCorda upbow upmordent upprall varcoda 
    verylongfermata voiceFour voiceFourStyle voiceNeutralStyle voiceOne 
    voiceOneStyle voiceThree voiceThreeStyle voiceTwo voiceTwoStyle xNotesOff))

(defconst lyqi:lilypond-music-functions
  '(acciaccatura addChordShape addInstrumentDefinition addQuote afterGrace 
    allowPageTurn applyContext applyMusic applyOutput appoggiatura 
    assertBeamQuant assertBeamSlope autochange balloonGrobText balloonText bar 
    barNumberCheck bendAfter bookOutputName bookOutputSuffix breathe clef 
    cueDuring deadNote defaultNoteHeads displayLilyMusic displayMusic 
    endSpanners featherDurations grace harmonicNote harmonicsOn 
    instrumentSwitch keepWithTag killCues label makeClusters musicMap 
    noPageBreak noPageTurn octaveCheck ottava overrideBeamSettings 
    overrideProperty pageBreak pageTurn palmMute palmMuteOn parallelMusic 
    parenthesize partcombine phrasingSlurDashPattern pitchedTrill 
    pointAndClickOff pointAndClickOn quoteDuring removeWithTag 
    resetRelativeOctave revertBeamSettings rightHandFinger scaleDurations 
    setBeatGrouping shiftDurations slurDashPattern spacingTweaks 
    storePredefinedDiagram styledNoteHeads tabChordRepetition tag 
    tieDashPattern tocItem transposedCueDuring transposition tweak 
    unfoldRepeats withMusicProperty xNote xNotesOn))

(defconst lyqi:lilypond-markup-commands
  '(abs-fontsize arrow-head backslashed-digit beam bold box bracket caps 
    center-align center-column char circle column column-lines-list combine 
    concat customTabClef dir-column doubleflat doublesharp draw-circle 
    draw-line dynamic epsfile eyeglasses fill-line filled-box finger flat 
    fontCaps fontsize fraction fret-diagram fret-diagram-terse 
    fret-diagram-verbose fromproperty general-align halign harp-pedal hbracket 
    hcenter-in hspace huge italic justified-lines-list justify justify-field 
    justify-string large larger left-align left-brace left-column line lookup 
    lower magnify markalphabet markletter medium musicglyph natural 
    normal-size-sub normal-size-super normal-text normalsize note 
    note-by-number null number on-the-fly override override-lines-list 
    pad-around pad-markup pad-to-box pad-x page-ref parenthesize postscript 
    put-adjacent raise right-align right-brace right-column roman rotate 
    rounded-box sans score semiflat semisharp sesquiflat sesquisharp sharp 
    simple slashed-digit small smallCaps smaller stencil strut sub super 
    table-of-contents-list teeny text tied-lyric tiny translate 
    translate-scaled transparent triangle typewriter underline upright vcenter 
    verbatim-file vspace whiteout with-color with-dimensions with-url wordwrap 
    wordwrap-field wordwrap-internal-list wordwrap-lines-list wordwrap-string 
    wordwrap-string-internal-list))

(defconst lyqi:lilypond-markup-list-commands
  '(column-lines-markup-list justified-lines-markup-list 
    override-lines-markup-list table-of-contents-markup-list 
    wordwrap-internal-markup-list wordwrap-lines-markup-list 
    wordwrap-string-internal-markup-list))

(defconst lyqi:scheme-lily-procedures
  '(!= _ accidental-interface::calc-alteration add-grace-property add-lyrics 
    add-music add-music-fonts add-new-clef add-pango-fonts add-point 
    add-quotable add-score add-stroke-glyph add-stroke-straight add-text 
    adjust-slash-stencil alist->hash-table alist<? all-bar-numbers-visible 
    all-repeat-counts-visible alterations-in-key ambitus::print 
    annotate-spacing-spec annotate-y-interval assoc-get average backend-testing 
    banter-chord-names bar-check bar-line::calc-break-visibility 
    bar-line::calc-glyph-name bend::print binary-search boolean-or-symbol? 
    box-grob-stencil box-stencil bracketify-stencil cached-file-contents 
    call-pure-function car< centered-stencil chain-assoc-get 
    chain-grob-member-functions cheap-list? check-quant-callbacks 
    check-slope-callbacks circle-stencil clef::print-modern-tab-if-set 
    collect-book-music-for-book collect-bookpart-for-book collect-music-aux 
    collect-music-for-book collect-scores-for-book color? 
    column-lines-markup-list completize-formats construct-chord-elements 
    context-change context-spec-music context-specification count-list 
    create-glyph-flag cue-substitute debugf decode-byte-string 
    default-auto-beam-check default-dynamic-absolute-volume default-flag 
    default-instrument-equalizer descend-to-context determine-frets 
    determine-split-list dimension-arrows dir-basename display-lily-music 
    display-music display-scheme-music dots::calc-dot-count 
    dots::calc-staff-position dump-gc-protects dump-live-object-stats 
    dynamic-text-spanner::before-line-breaking ellipse-stencil empty-music 
    eps-file->stencil ergonomic-simple-format eval-carefully event-cause 
    event-chord event-chord-notes event-chord-pitches 
    every-nth-bar-number-visible every-nth-repeat-count-visible 
    extract-named-music fancy-format filtered-map find-pitch-entry 
    fingering::calc-text first-assoc first-bar-number-invisible first-member 
    flatten-alist font-name-style format-bass-figure format-mark-alphabet 
    format-mark-barnumbers format-mark-box-alphabet format-mark-box-barnumbers 
    format-mark-box-letters format-mark-box-numbers format-mark-circle-alphabet 
    format-mark-circle-barnumbers format-mark-circle-letters 
    format-mark-circle-numbers format-mark-letters format-mark-numbers 
    four-string-banjo fret-board::calc-stencil fret-letter-tablature-format 
    fret-number-tablature-format fret-number-tablature-format-banjo 
    fret-parse-terse-definition-string get-chord-shape 
    glissando::calc-tab-extra-dy glyph-flag 
    grace-spacing::calc-shortest-duration grob-list? 
    grob::calc-property-by-copy grob::has-interface grob::is-live? gui-main 
    hairpin::calc-grow-direction hash-table->alist horizontal-slash-interval 
    ignatzek-chord-names interpret-markup-list interval-bound interval-center 
    interval-empty? interval-end interval-index interval-intersection 
    interval-length interval-sane? interval-start interval-translate 
    interval-union interval-widen is-absolute? jazz-chord-names 
    justified-lines-markup-list key-signature-interface::alteration-position 
    layout-extract-page-properties layout-set-absolute-staff-size 
    layout-set-absolute-staff-size-in-module layout-set-staff-size lilypond-all 
    lilypond-main lilypond-version list-insert-separator list-join 
    list-or-symbol? log2 lookup-markup-command lookup-markup-list-command 
    ly-getcwd ly:accidental-interface::height ly:accidental-interface::print 
    ly:accidental-interface::pure-height ly:accidental-interface::width 
    ly:accidental-placement::calc-positioning-done ly:add-context-mod 
    ly:add-file-name-alist ly:add-interface ly:add-listener ly:add-option 
    ly:align-interface::align-to-ideal-distances 
    ly:align-interface::align-to-minimum-distances ly:all-grob-interfaces 
    ly:all-options ly:all-output-backend-commands ly:all-stencil-commands 
    ly:all-stencil-expressions ly:apply-context-iterator::constructor 
    ly:arpeggio::brew-chord-bracket ly:arpeggio::brew-chord-slur 
    ly:arpeggio::calc-positions ly:arpeggio::print ly:arpeggio::pure-height 
    ly:arpeggio::width ly:assoc-get ly:auto-change-iterator::constructor 
    ly:axis-group-interface::add-element 
    ly:axis-group-interface::adjacent-pure-heights 
    ly:axis-group-interface::calc-next-staff-spacing 
    ly:axis-group-interface::calc-skylines 
    ly:axis-group-interface::calc-x-common 
    ly:axis-group-interface::calc-y-common 
    ly:axis-group-interface::combine-skylines ly:axis-group-interface::height 
    ly:axis-group-interface::print ly:axis-group-interface::pure-height 
    ly:axis-group-interface::width ly:balloon-interface::print 
    ly:bar-check-iterator::constructor ly:bar-line::calc-anchor 
    ly:bar-line::calc-bar-extent ly:bar-line::calc-bar-size ly:bar-line::print 
    ly:beam-grouping ly:beam::calc-beaming ly:beam::calc-concaveness 
    ly:beam::calc-cross-staff ly:beam::calc-direction 
    ly:beam::calc-least-squares-positions ly:beam::calc-normal-stems 
    ly:beam::calc-stem-shorten ly:beam::print ly:beam::quanting 
    ly:beam::rest-collision-callback ly:beam::set-stem-lengths 
    ly:beam::shift-region-to-valid ly:beam::slope-damping ly:beat-grouping 
    ly:book-add-bookpart! ly:book-add-score! ly:book-process 
    ly:book-process-to-systems ly:box? ly:bp ly:bracket 
    ly:break-alignable-interface::self-align-callback 
    ly:break-aligned-interface::calc-average-anchor 
    ly:break-aligned-interface::calc-break-visibility 
    ly:break-aligned-interface::calc-extent-aligned-anchor 
    ly:break-alignment-interface::calc-positioning-done 
    ly:breathing-sign::divisio-maior ly:breathing-sign::divisio-maxima 
    ly:breathing-sign::divisio-minima ly:breathing-sign::finalis 
    ly:breathing-sign::offset-callback ly:broadcast 
    ly:camel-case->lisp-identifier ly:chain-assoc-get 
    ly:change-iterator::constructor ly:chord-name::after-line-breaking 
    ly:chord-tremolo-iterator::constructor ly:clear-anonymous-modules 
    ly:clef::calc-glyph-name ly:clef::print ly:cluster-beacon::height 
    ly:cluster::calc-cross-staff ly:cluster::print ly:cm ly:command-line-code 
    ly:command-line-options ly:command-line-verbose? ly:connect-dispatchers 
    ly:context-current-moment ly:context-event-source ly:context-events-below 
    ly:context-find ly:context-grob-definition ly:context-id ly:context-name 
    ly:context-now ly:context-parent ly:context-property 
    ly:context-property-where-defined ly:context-pushpop-property 
    ly:context-set-property! ly:context-specced-music-iterator::constructor 
    ly:context-unset-property ly:context? ly:custos::print ly:default-scale 
    ly:dimension? ly:dir? ly:dispatcher? ly:dot-column::calc-positioning-done 
    ly:dots::print ly:duration->string ly:duration-dot-count ly:duration-factor 
    ly:duration-length ly:duration-log ly:duration::less? ly:duration<? 
    ly:duration? ly:effective-prefix ly:enclosing-bracket::print 
    ly:enclosing-bracket::width ly:engraver-make-grob ly:error 
    ly:eval-simple-closure ly:event-chord-iterator::constructor 
    ly:event-deep-copy ly:event-iterator::constructor ly:event-property 
    ly:event-set-property! ly:expand-environment ly:export 
    ly:figured-bass-continuation::center-on-figures 
    ly:figured-bass-continuation::print ly:find-file 
    ly:font-config-add-directory ly:font-config-add-font 
    ly:font-config-display-fonts ly:font-config-get-font-file 
    ly:font-design-size ly:font-file-name ly:font-get-glyph 
    ly:font-glyph-name-to-charcode ly:font-glyph-name-to-index 
    ly:font-index-to-charcode ly:font-magnification ly:font-metric? 
    ly:font-name ly:font-sub-fonts ly:format ly:format-output 
    ly:get-all-function-documentation ly:get-all-translators 
    ly:get-context-mods ly:get-listened-event-classes ly:get-option ly:gettext 
    ly:grace-iterator::constructor ly:grace-music::start-callback 
    ly:grid-line-interface::print ly:grid-line-interface::width 
    ly:grob-alist-chain ly:grob-array-length ly:grob-array-ref ly:grob-array? 
    ly:grob-basic-properties ly:grob-common-refpoint 
    ly:grob-common-refpoint-of-array ly:grob-default-font ly:grob-extent 
    ly:grob-interfaces ly:grob-layout ly:grob-object ly:grob-original 
    ly:grob-parent ly:grob-pq<? ly:grob-properties ly:grob-property 
    ly:grob-property-data ly:grob-relative-coordinate 
    ly:grob-robust-relative-extent ly:grob-script-priority-less 
    ly:grob-set-nested-property! ly:grob-set-object! ly:grob-set-parent! 
    ly:grob-set-property! ly:grob-staff-position ly:grob-suicide! 
    ly:grob-system ly:grob-translate-axis! ly:grob::stencil-height 
    ly:grob::stencil-width ly:grob::x-parent-positioning 
    ly:grob::y-parent-positioning ly:grob? ly:grouping-rules ly:gulp-file 
    ly:hairpin::print ly:hairpin::pure-height 
    ly:hara-kiri-group-spanner::calc-skylines 
    ly:hara-kiri-group-spanner::force-hara-kiri-callback 
    ly:hara-kiri-group-spanner::force-hara-kiri-in-y-parent-callback 
    ly:hara-kiri-group-spanner::pure-height 
    ly:hara-kiri-group-spanner::y-extent ly:hash-table-keys 
    ly:horizontal-bracket::print ly:in-event-class? ly:inch ly:inexact->string 
    ly:input-both-locations ly:input-file-line-char-column ly:input-location? 
    ly:input-message ly:interpret-music-expression 
    ly:interpret-stencil-expression ly:intlog2 ly:is-listened-event-class 
    ly:item-break-dir ly:item? ly:iterator? ly:key-signature-interface::print 
    ly:ledger-line-spanner::print ly:ledger-line-spanner::set-spacing-rods 
    ly:lexer-keywords ly:lily-lexer? ly:lily-parser? 
    ly:line-spanner::calc-left-bound-info 
    ly:line-spanner::calc-left-bound-info-and-text 
    ly:line-spanner::calc-right-bound-info ly:line-spanner::print 
    ly:list->offsets ly:listener? ly:load 
    ly:lyric-combine-music-iterator::constructor 
    ly:lyric-combine-music::length-callback ly:lyric-extender::print 
    ly:lyric-hyphen::print ly:lyric-hyphen::set-spacing-rods ly:make-book 
    ly:make-book-part ly:make-dispatcher ly:make-duration ly:make-event-class 
    ly:make-global-context ly:make-global-translator ly:make-listener 
    ly:make-moment ly:make-music ly:make-music-function ly:make-output-def 
    ly:make-page-label-marker ly:make-page-permission-marker 
    ly:make-pango-description-string ly:make-paper-outputter ly:make-pitch 
    ly:make-prob ly:make-scale ly:make-score ly:make-simple-closure 
    ly:make-stencil ly:make-stream-event ly:measure-grouping::print 
    ly:melody-spanner::calc-neutral-stem-direction 
    ly:mensural-ligature::brew-ligature-primitive ly:mensural-ligature::print 
    ly:message ly:minimal-breaking ly:mm ly:module->alist ly:module-copy 
    ly:modules-lookup ly:moment-add ly:moment-div ly:moment-grace-denominator 
    ly:moment-grace-numerator ly:moment-main-denominator 
    ly:moment-main-numerator ly:moment-mod ly:moment-mul ly:moment-sub 
    ly:moment<? ly:moment? ly:multi-measure-rest::percent 
    ly:multi-measure-rest::print ly:multi-measure-rest::set-spacing-rods 
    ly:multi-measure-rest::set-text-rods ly:music-compress ly:music-deep-copy 
    ly:music-duration-compress ly:music-duration-length 
    ly:music-function-extract ly:music-function? ly:music-iterator::constructor 
    ly:music-length ly:music-list? ly:music-message ly:music-mutable-properties 
    ly:music-output? ly:music-property 
    ly:music-sequence::cumulative-length-callback 
    ly:music-sequence::event-chord-relative-callback 
    ly:music-sequence::first-start-callback 
    ly:music-sequence::maximum-length-callback 
    ly:music-sequence::minimum-start-callback 
    ly:music-sequence::repeated-chord-relative-callback 
    ly:music-sequence::simultaneous-relative-callback ly:music-set-property! 
    ly:music-transpose ly:music-wrapper-iterator::constructor 
    ly:music-wrapper::length-callback ly:music-wrapper::start-callback 
    ly:music::duration-length-callback ly:music? 
    ly:note-collision-interface::calc-positioning-done 
    ly:note-column-accidentals ly:note-column-dot-column 
    ly:note-head::calc-stem-attachment ly:note-head::print 
    ly:note-head::stem-attachment ly:note-head::stem-x-shift ly:number->string 
    ly:number-pair->string ly:optimal-breaking ly:option-usage ly:otf->cff 
    ly:otf-font-glyph-info ly:otf-font-table-data ly:otf-font? 
    ly:otf-glyph-count ly:otf-glyph-list ly:ottava-bracket::print 
    ly:output-def-clone ly:output-def-lookup ly:output-def-parent 
    ly:output-def-scope ly:output-def-set-variable! ly:output-def? 
    ly:output-description ly:output-formats ly:outputter-close 
    ly:outputter-dump-stencil ly:outputter-dump-string 
    ly:outputter-output-scheme ly:outputter-port ly:page-marker? 
    ly:page-turn-breaking ly:pango-font-physical-fonts ly:pango-font? 
    ly:paper-book-pages ly:paper-book-paper ly:paper-book-performances 
    ly:paper-book-scopes ly:paper-book-systems ly:paper-book? 
    ly:paper-column::before-line-breaking ly:paper-column::print ly:paper-fonts 
    ly:paper-get-font ly:paper-get-number ly:paper-outputscale 
    ly:paper-score-paper-systems ly:paper-system-minimum-distance 
    ly:paper-system? ly:parse-file ly:parser-clear-error ly:parser-clone 
    ly:parser-define! ly:parser-error ly:parser-has-error? ly:parser-lexer 
    ly:parser-lookup ly:parser-output-name ly:parser-parse-string 
    ly:parser-set-note-names ly:parser-set-repetition-function 
    ly:parser-set-repetition-symbol ly:part-combine-iterator::constructor 
    ly:percent-repeat-item-interface::beat-slash 
    ly:percent-repeat-item-interface::double-percent 
    ly:percent-repeat-iterator::constructor ly:performance-write ly:pfb->pfa 
    ly:piano-pedal-bracket::print ly:pitch-alteration ly:pitch-diff 
    ly:pitch-negate ly:pitch-notename ly:pitch-octave ly:pitch-quartertones 
    ly:pitch-semitones ly:pitch-steps ly:pitch-transpose ly:pitch::less? 
    ly:pitch<? ly:pitch? ly:pointer-group-interface::add-grob 
    ly:pop-property-iterator::constructor ly:position-on-line? 
    ly:prob-immutable-properties ly:prob-mutable-properties ly:prob-property 
    ly:prob-property? ly:prob-set-property! ly:prob-type? ly:prob? 
    ly:programming-error ly:progress ly:property-iterator::constructor 
    ly:property-iterator::once-finalization ly:property-lookup-stats 
    ly:property-unset-iterator::constructor ly:protects ly:pt 
    ly:push-property-iterator::constructor 
    ly:push-property-iterator::once-finalization ly:quote-iterator::constructor 
    ly:register-stencil-expression ly:relative-group-extent 
    ly:relative-octave-check::relative-callback 
    ly:relative-octave-music::no-relative-callback 
    ly:relative-octave-music::relative-callback ly:repeated-music::first-start 
    ly:repeated-music::folded-music-length ly:repeated-music::minimum-start 
    ly:repeated-music::relative-callback 
    ly:repeated-music::unfolded-music-length 
    ly:repeated-music::volta-music-length ly:reset-all-fonts 
    ly:rest-collision::calc-positioning-done 
    ly:rest-collision::force-shift-callback 
    ly:rest-collision::force-shift-callback-rest ly:rest::calc-cross-staff 
    ly:rest::height ly:rest::print ly:rest::pure-height ly:rest::width 
    ly:rest::y-offset-callback ly:round-filled-box ly:round-filled-polygon 
    ly:run-translator ly:score-add-output-def! ly:score-embedded-format 
    ly:score-error? ly:score-header ly:score-music ly:score-output-defs 
    ly:score-set-header! ly:score? ly:script-column::before-line-breaking 
    ly:script-column::row-before-line-breaking 
    ly:script-interface::calc-cross-staff ly:script-interface::calc-direction 
    ly:script-interface::calc-positioning-done ly:script-interface::print 
    ly:self-alignment-interface::aligned-on-x-parent 
    ly:self-alignment-interface::aligned-on-y-parent 
    ly:self-alignment-interface::centered-on-x-parent 
    ly:self-alignment-interface::centered-on-y-parent 
    ly:self-alignment-interface::pure-y-aligned-on-self 
    ly:self-alignment-interface::x-aligned-on-self 
    ly:self-alignment-interface::x-centered-on-y-parent 
    ly:self-alignment-interface::y-aligned-on-self 
    ly:semi-tie-column::calc-head-direction 
    ly:semi-tie-column::calc-positioning-done ly:semi-tie::calc-control-points 
    ly:separation-item::calc-skylines ly:separation-item::print 
    ly:sequential-iterator::constructor ly:set-default-scale 
    ly:set-grob-modification-callback ly:set-middle-C! ly:set-option 
    ly:set-property-cache-callback ly:side-position-interface::calc-cross-staff 
    ly:side-position-interface::move-to-extremal-staff 
    ly:side-position-interface::pure-y-aligned-on-support-refpoints 
    ly:side-position-interface::pure-y-aligned-side 
    ly:side-position-interface::x-aligned-side 
    ly:side-position-interface::y-aligned-on-support-refpoints 
    ly:side-position-interface::y-aligned-side ly:simple-closure? 
    ly:simple-music-iterator::constructor ly:simplify-scheme 
    ly:simultaneous-music-iterator::constructor ly:skyline-pair? ly:skyline? 
    ly:slur::calc-control-points ly:slur::calc-cross-staff 
    ly:slur::calc-direction ly:slur::height ly:slur::outside-slur-callback 
    ly:slur::outside-slur-cross-staff ly:slur::print ly:slur::pure-height 
    ly:slur::pure-outside-slur-callback ly:smob-protects 
    ly:solve-spring-rod-problem ly:source-file? 
    ly:spacing-spanner::calc-common-shortest-duration 
    ly:spacing-spanner::set-springs ly:span-bar::before-line-breaking 
    ly:span-bar::calc-bar-size ly:span-bar::calc-glyph-name 
    ly:span-bar::center-on-spanned-callback ly:span-bar::print 
    ly:span-bar::width ly:spanner-bound ly:spanner-broken-into 
    ly:spanner::bounds-width ly:spanner::kill-zero-spanned-time 
    ly:spanner::set-spacing-rods ly:spanner? ly:staff-symbol-line-thickness 
    ly:staff-symbol-referencer::callback ly:staff-symbol-staff-space 
    ly:staff-symbol::height ly:staff-symbol::print ly:start-environment 
    ly:stderr-redirect ly:stem-tremolo::calc-slope ly:stem-tremolo::calc-style 
    ly:stem-tremolo::calc-width ly:stem-tremolo::height ly:stem-tremolo::print 
    ly:stem-tremolo::width ly:stem::calc-cross-staff 
    ly:stem::calc-default-direction ly:stem::calc-direction ly:stem::calc-flag 
    ly:stem::calc-length ly:stem::calc-positioning-done 
    ly:stem::calc-stem-end-position ly:stem::calc-stem-info ly:stem::height 
    ly:stem::offset-callback ly:stem::print ly:stem::pure-height ly:stem::width 
    ly:stencil-add ly:stencil-aligned-to ly:stencil-combine-at-edge 
    ly:stencil-empty? ly:stencil-expr ly:stencil-extent ly:stencil-fonts 
    ly:stencil-in-color ly:stencil-rotate ly:stencil-rotate-absolute 
    ly:stencil-translate ly:stencil-translate-axis ly:stencil? 
    ly:stream-event::dump ly:stream-event::undump ly:stream-event? 
    ly:string-percent-encode ly:string-substitute ly:sustain-pedal::print 
    ly:system ly:system-font-load ly:system-start-delimiter::print 
    ly:text-interface::interpret-string ly:text-interface::print 
    ly:tie-column::before-line-breaking ly:tie-column::calc-positioning-done 
    ly:tie::calc-control-points ly:tie::calc-direction ly:tie::print 
    ly:time-signature::print ly:translator-context ly:translator-description 
    ly:translator-group? ly:translator-name ly:translator? 
    ly:transpose-key-alist ly:truncate-list! ly:ttf->pfa ly:ttf-ps-name 
    ly:tuplet-bracket::calc-connect-to-neighbors 
    ly:tuplet-bracket::calc-control-points ly:tuplet-bracket::calc-cross-staff 
    ly:tuplet-bracket::calc-direction ly:tuplet-bracket::calc-positions 
    ly:tuplet-bracket::print ly:tuplet-iterator::constructor 
    ly:tuplet-number::calc-cross-staff ly:tuplet-number::print 
    ly:unfolded-repeat-iterator::constructor ly:unit ly:usage 
    ly:vaticana-ligature::brew-ligature-primitive ly:vaticana-ligature::print 
    ly:version ly:volta-bracket-interface::print 
    ly:volta-repeat-iterator::constructor ly:warning ly:wide-char->utf-8 
    lyric-combine lyric-text::print magnification->font-size magstep 
    make-accidental-rule make-apply-context make-articulation 
    make-autochange-music make-beam-rule-time-signature-set 
    make-century-schoolbook-tree make-circle-stencil make-clef-set 
    make-column-lines-markup-list make-duration-of-length make-ellipse-stencil 
    make-event-chord make-filled-box-stencil make-grace-music 
    make-grob-property-override make-grob-property-revert 
    make-grob-property-set make-justified-lines-markup-list make-line-stencil 
    make-lyric-event make-mark-set make-multi-measure-rest make-music 
    make-non-relative-music make-ottava-set make-oval-stencil 
    make-override-lines-markup-list make-pango-font-tree 
    make-part-combine-music make-property-set make-property-unset make-repeat 
    make-repeated-music make-safe-lilypond-module make-sequential-music 
    make-simultaneous-music make-skip-music make-span-event make-stencil-boxer 
    make-stencil-circler make-time-signature-set make-type-checker 
    make-voice-props-revert make-voice-props-set 
    make-wordwrap-internal-markup-list make-wordwrap-lines-markup-list 
    make-wordwrap-string-internal-markup-list map-markup-command-list 
    marked-up-headfoot marked-up-title markup-command-keyword 
    markup-command-list? markup-command-signature markup-command-signature-ref 
    markup-command-signature-set! markup-list? markup-signature-to-keyword 
    markup? mensural-flag midi-program mmrest-of-length modern-straight-flag 
    modified-font-metric-font-scaling modulo-bar-number-visible moment-min 
    moment-pair? moment<=? multi-measure-rest music->make-music music-clone 
    music-filter music-function music-has-type music-is-of-type? music-map 
    music-separator? myd neo-modern-accidental-rule no-flag normal-flag 
    note-head::brew-ez-stencil note-head::calc-duration-log 
    note-head::calc-glyph-name note-name->markup note-to-cluster 
    notes-to-clusters number-or-grob? number-or-string? number-pair? 
    object-type object-type-name offset-add offset-flip-y offset-fret 
    offset-scale old-relative-not-used-message old-straight-flag ordered-cons 
    output-scopes outputproperty-compatibility oval-stencil 
    override-beam-setting override-head-style override-lines-markup-list 
    override-property-setting parentheses-item::calc-angled-bracket-stencils 
    parentheses-item::calc-parenthesis-stencils 
    parentheses-item::calc-tabstaff-parenthesis-stencils parenthesize-stencil 
    parenthesize-tab-note-head parse-string-result parse-terse-string 
    percussion? pitch-of-note postprocess-output postscript->pdf 
    postscript->png prepend-alist-chain print print-book-with-defaults 
    print-book-with-defaults-as-systems print-circled-text-callback 
    property-operation pure-relevant? quote-substitute read-lily-expression 
    recording-group-emulate relevant-book-systems relevant-dump-systems 
    remove-grace-property remove-stencil-warnings repeat 
    repeat-tie::handle-tab-note-head repetition-chord retrieve-glyph-flag 
    revert-beam-setting revert-head-style rgb-color rounded-box-stencil 
    sanitize-command-option scale-layout scheme? scm->string 
    score-override-beam-setting scorify-music script-interface::calc-x-offset 
    search-executable search-gs select-head-glyph sequential-music 
    sequential-music-to-chord-exceptions set-accidental-style 
    set-accidentals-properties set-bar-number-visibility set-default-paper-size 
    set-global-staff-size set-mus-properties! set-octavation 
    set-output-property set-paper-dimension-variables set-paper-size 
    set-time-signature shift-duration-log shift-one-duration-log 
    shift-right-at-line-begin simultaneous-music skip->rest skip-music 
    skip-of-length slur::draw-tab-slur smart-bar-check space-lines 
    split-list-by-separator stack-lines stack-stencil-line stack-stencils 
    stack-stencils-padding-list stderr stem::calc-duration-log stencil-whiteout 
    stencil-with-color straight-flag string-encode-integer string-endswith 
    string-number::calc-text string-or-pair? string-or-symbol? 
    string-regexp-substitute string-startswith stroke-finger::calc-text 
    style-note-heads symbol-key<? symbol<? symmetric-interval 
    system-start-text::calc-x-offset system-start-text::calc-y-offset 
    system-start-text::print system-with-env tab-note-head::calc-glyph-name 
    tab-note-head::whiteout-if-style-set tablature-position-on-lines 
    tabvoice::draw-double-stem-for-half-notes teaching-accidental-rule tempo 
    tempoText tie::handle-tab-note-head time-scaled-music transpose-music 
    tuplet-number::append-note-wrapper tuplet-number::calc-denominator-text 
    tuplet-number::calc-fraction-text tuplet-number::fraction-with-notes 
    tuplet-number::non-default-fraction-with-notes 
    tuplet-number::non-default-tuplet-denominator-text 
    tuplet-number::non-default-tuplet-fraction-text type-name 
    ugh-compat-double-plus-new-chord->markup unfold-repeats uniq-list 
    uniqued-alist unrelativable-music vector-for-each version-not-seen-message 
    voice-separator voicify-music void-music wordwrap-internal-markup-list 
    wordwrap-lines-markup-list wordwrap-string-internal-markup-list write-me 
    write-performances-midis write-system-signature write-system-signatures 
    x11-color))

(defconst lyqi:scheme-lily-macros
  '(_i def-grace-function define-markup-command define-markup-list-command 
    define-music-function make-stream-event markup markup*))

(defconst lyqi:scheme-lily-variables
  '(CENTER DOS DOUBLE-FLAT DOUBLE-FLAT-QTS DOUBLE-SHARP DOUBLE-SHARP-QTS DOWN 
    FLAT FLAT-QTS LEFT NATURAL NATURAL-QTS PLATFORM RIGHT SEMI-FLAT 
    SEMI-FLAT-QTS SEMI-SHARP SEMI-SHARP-QTS SEMI-TONE SEMI-TONE-QTS SHARP 
    SHARP-QTS START STOP THREE-Q-FLAT THREE-Q-FLAT-QTS THREE-Q-SHARP 
    THREE-Q-SHARP-QTS UP X Y ZERO-MOMENT absolute-volume-alist 
    all-backend-properties all-grob-descriptions all-internal-grob-properties 
    all-internal-translation-properties all-invisible all-music-font-encodings 
    all-music-properties all-text-font-encodings all-translation-properties 
    all-user-grob-properties all-user-translation-properties all-visible 
    alteration-default-glyph-name-alist alteration-hufnagel-glyph-name-alist 
    alteration-medicaea-glyph-name-alist alteration-mensural-glyph-name-alist 
    alteration-vaticana-glyph-name-alist banjo-c-tuning banjo-modal-tuning 
    banjo-open-d-tuning banjo-open-dm-tuning banjo-open-g-tuning 
    bass-drop-d-tuning bass-five-string-tuning bass-four-string-tuning 
    bass-six-string-tuning bass-tuning begin-of-line-invisible 
    begin-of-line-visible black blue cancellation-glyph-name-alist 
    center-invisible center-visible current-outfile-name cyan darkblue darkcyan 
    darkgreen darkmagenta darkred darkyellow default-beam-settings 
    default-chord-modifier-list default-melisma-properties default-script-alist 
    dynamic-default-volume empty-interval empty-markup empty-stencil 
    end-of-line-invisible end-of-line-visible feta-design-size-mapping green 
    grey guitar-asus4-tuning guitar-dadgad-tuning guitar-drop-d-tuning 
    guitar-lute-tuning guitar-open-d-tuning guitar-open-g-tuning 
    guitar-seven-string-tuning guitar-tuning instrument-equalizer-alist 
    latin1-coding-vector lily-unit->bigpoint-factor lily-unit->mm-factor 
    magenta makam-alteration-glyph-name-alist mandolin-tuning 
    markup-functions-by-category markup-functions-properties 
    markup-list-functions music-descriptions music-name-to-property-table 
    paper-alist parser point-stencil red spanbar-begin-of-line-invisible 
    standard-alteration-glyph-name-alist supported-clefs 
    toplevel-music-functions ukulele-baritone-tuning ukulele-d-tuning 
    ukulele-tenor-tuning ukulele-tuning white yellow))

(defconst lyqi:scheme-guile-procedures
  '($abs $acos $acosh $asin $asinh $atan $atan2 $atanh $cos $cosh $exp $expt 
    $log $sin $sinh $sqrt $tan $tanh %get-pre-modules-obarray %get-stack-size 
    %init-goops-builtins %init-rdelim-builtins %init-rw-builtins 
    %init-weaks-builtins %library-dir %load-announce %load-hook %make-void-port 
    %package-data-dir %print-module %print-values %record-type-error 
    %search-load-path %site-dir * + - ->bool ->char-set / 1+ 1- < <= = > >= abs 
    accept access? acons acos acosh add-hook! alarm all-threads and-map and=> 
    angle any->c32vector any->c64vector any->f32vector any->f64vector 
    any->s16vector any->s32vector any->s64vector any->s8vector any->u16vector 
    any->u32vector any->u64vector any->u8vector append append! apply 
    apply-to-args apply:nconc2last array->list array-contents array-copy! 
    array-copy-in-order! array-dimensions array-equal? array-fill! 
    array-for-each array-in-bounds? array-index-map! array-map! 
    array-map-in-order! array-prototype array-rank array-ref array-set! 
    array-shape array-type array? ash asin asinh assert-defmacro?! 
    assert-load-verbosity assert-repl-print-unspecified assert-repl-silence 
    assert-repl-verbosity assoc assoc-ref assoc-remove! assoc-set! assq 
    assq-ref assq-remove! assq-set! assv assv-ref assv-remove! assv-set! async 
    async-mark atan atanh autoload-done! autoload-done-or-in-progress? 
    autoload-in-progress! backtrace bad-throw basename basic-load batch-mode? 
    beautify-user-module! bind bind-textdomain-codeset bindtextdomain bit-count 
    bit-count* bit-extract bit-invert! bit-position bit-set*! bitvector 
    bitvector->list bitvector-fill! bitvector-length bitvector-ref 
    bitvector-set! bitvector? boolean? broadcast-condition-variable 
    builtin-variable c-clear-registered-modules c-registered-modules c32vector 
    c32vector->list c32vector-length c32vector-ref c32vector-set! c32vector? 
    c64vector c64vector->list c64vector-length c64vector-ref c64vector-set! 
    c64vector? caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar 
    caddar cadddr caddr cadr call-with-blocked-asyncs 
    call-with-current-continuation call-with-deferred-observers 
    call-with-dynamic-root call-with-input-file call-with-input-string 
    call-with-new-thread call-with-output-file call-with-output-string 
    call-with-unblocked-asyncs call-with-values call/cc car catch cdaaar cdaadr 
    cdaar cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr cddr 
    cdr ceiling char->integer char-alphabetic? char-ci<=? char-ci<? char-ci=? 
    char-ci>=? char-ci>? char-downcase char-is-both? char-lower-case? 
    char-numeric? char-ready? char-set char-set->list char-set->string 
    char-set-adjoin char-set-adjoin! char-set-any char-set-complement 
    char-set-complement! char-set-contains? char-set-copy char-set-count 
    char-set-cursor char-set-cursor-next char-set-delete char-set-delete! 
    char-set-diff+intersection char-set-diff+intersection! char-set-difference 
    char-set-difference! char-set-every char-set-filter char-set-filter! 
    char-set-fold char-set-for-each char-set-hash char-set-intersection 
    char-set-intersection! char-set-map char-set-ref char-set-size 
    char-set-unfold char-set-unfold! char-set-union char-set-union! 
    char-set-xor char-set-xor! char-set<= char-set= char-set? char-upcase 
    char-upper-case? char-whitespace? char<=? char<? char=? char>=? char>? 
    char? chdir chmod chown chroot close close-all-ports-except close-fdes 
    close-input-port close-io-port close-output-port close-port closedir 
    closure? command-line compile-define-module-args compile-interface-spec 
    complex? cond-expand-provide connect cons cons* cons-source 
    convert-c-registered-modules copy-file copy-random-state copy-tree cos cosh 
    crypt ctermid current-dynamic-state current-error-port current-input-port 
    current-load-port current-module current-output-port current-thread 
    current-time debug-disable debug-enable debug-object? debug-options 
    debug-options-interface default-duplicate-binding-handler 
    default-duplicate-binding-procedures default-lazy-handler defined? 
    defmacro-transformer defmacro:syntax-transformer defmacro:transformer 
    defmacro? delete delete! delete-file delete1! delq delq! delq1! delv delv! 
    delv1! denominator destroy-guardian! dimensions->uniform-array 
    directory-stream? dirname display display-application display-backtrace 
    display-error display-usage-report doubly-weak-hash-table? drain-input dup 
    dup->fdes dup->inport dup->outport dup->port dup2 duplicate-port 
    dynamic-args-call dynamic-call dynamic-func dynamic-link dynamic-maybe-call 
    dynamic-maybe-link dynamic-object? dynamic-root dynamic-state? 
    dynamic-unlink dynamic-wind effective-version enclose-array 
    end-of-char-set? endgrent endhostent endnetent endprotoent endpwent 
    endservent entity? env-module environ environment-bound? environment-cell 
    environment-define environment-fold environment-module environment-observe 
    environment-observe-weak environment-ref environment-set! 
    environment-undefine environment-unobserve environment? eof-object? eq? 
    equal? eqv? error error-catching-loop error-catching-repl eval eval-disable 
    eval-enable eval-environment-imported eval-environment-local 
    eval-environment-set-imported! eval-environment-set-local! 
    eval-environment? eval-options eval-options-interface eval-string 
    evaluator-traps-interface even? exact->inexact exact? execl execle execlp 
    exit exp export-environment-private export-environment-set-private! 
    export-environment-set-signature! export-environment-signature 
    export-environment? expt f32vector f32vector->list f32vector-length 
    f32vector-ref f32vector-set! f32vector? f64vector f64vector->list 
    f64vector-length f64vector-ref f64vector-set! f64vector? fcntl fdes->inport 
    fdes->outport fdes->ports fdopen feature? file-exists? file-is-directory? 
    file-port? file-position file-set-position fileno filter filter! 
    find-and-link-dynamic-module flock floor fluid-ref fluid-set! fluid? 
    flush-all-ports for-each for-next-option force force-output format 
    frame-arguments frame-evaluating-args? frame-next frame-number 
    frame-overflow? frame-previous frame-procedure frame-procedure? frame-real? 
    frame-source frame? fsync ftell gc gc-live-object-stats gc-run-time 
    gc-stats gcd generalized-vector->list generalized-vector-length 
    generalized-vector-ref generalized-vector-set! generalized-vector? gensym 
    gentemp get-internal-real-time get-internal-run-time get-option 
    get-output-string get-print-state getcwd getegid getenv geteuid getgid 
    getgr getgrent getgrgid getgrnam getgroups gethost gethostbyaddr 
    gethostbyname gethostent gethostname getitimer getlogin getnet getnetbyaddr 
    getnetbyname getnetent getpass getpeername getpgrp getpid getppid 
    getpriority getproto getprotobyname getprotobynumber getprotoent getpw 
    getpwent getpwnam getpwuid getserv getservbyname getservbyport getservent 
    getsockname getsockopt gettext gettimeofday getuid gmtime group:gid 
    group:mem group:name group:passwd guardian-destroyed? guardian-greedy? 
    handle-system-error has-suffix? hash hash-clear! hash-create-handle! 
    hash-fold hash-for-each hash-for-each-handle hash-get-handle hash-map->list 
    hash-ref hash-remove! hash-set! hash-table? hashq hashq-create-handle! 
    hashq-get-handle hashq-ref hashq-remove! hashq-set! hashv 
    hashv-create-handle! hashv-get-handle hashv-ref hashv-remove! hashv-set! 
    hashx-create-handle! hashx-get-handle hashx-ref hashx-remove! hashx-set! 
    hook->list hook-empty? hook? hostent:addr-list hostent:addrtype 
    hostent:aliases hostent:length hostent:name htonl htons identity imag-part 
    import-environment-imports import-environment-set-imports! 
    import-environment? in-vicinity include-deprecated-features inet-aton 
    inet-lnaof inet-makeaddr inet-netof inet-ntoa inet-ntop inet-pton 
    inexact->exact inexact? inf inf? inherit-print-state init-dynamic-module 
    input-port? integer->char integer-expt integer-length integer? 
    interaction-environment intern-symbol iota isatty? 
    issue-deprecation-warning join-thread keyword->symbol keyword-dash-symbol 
    keyword-like-symbol->keyword keyword? kill kw-arg-ref last-pair 
    last-stack-frame lazy-catch lazy-handler-dispatch lcm leaf-environment? 
    length link link-dynamic-module list list* list->array list->bitvector 
    list->c32vector list->c64vector list->char-set list->char-set! 
    list->f32vector list->f64vector list->s16vector list->s32vector 
    list->s64vector list->s8vector list->string list->symbol list->typed-array 
    list->u16vector list->u32vector list->u64vector list->u8vector 
    list->uniform-array list->uniform-vector list->vector list-cdr-ref 
    list-cdr-set! list-copy list-head list-index list-ref list-set! list-tail 
    list? listen load load-emacs-interface load-extension load-from-path 
    load-module load-user-init local-define local-eval local-ref local-remove 
    local-set! localtime lock-mutex log log10 logand logbit? logcount logior 
    lognot logtest logxor lookup-duplicates-handlers lstat macro-name 
    macro-transformer macro-type macro? macroexpand macroexpand-1 magnitude 
    major-version make-arbiter make-array make-autoload-interface 
    make-bitvector make-c32vector make-c64vector make-class-object 
    make-condition-variable make-doubly-weak-hash-table 
    make-duplicates-interface make-dynamic-state make-eval-environment 
    make-export-environment make-f32vector make-f64vector make-fluid 
    make-guardian make-hash-table make-hook make-import-environment 
    make-keyword-from-dash-symbol make-leaf-environment make-list make-module 
    make-modules-in make-mutable-parameter make-mutex make-object-property 
    make-polar make-procedure-with-setter make-record-type make-rectangular 
    make-recursive-mutex make-regexp make-root-module make-s16vector 
    make-s32vector make-s64vector make-s8vector make-scm-module 
    make-shared-array make-socket-address make-soft-port make-stack make-string 
    make-struct make-struct-layout make-subclass-object make-symbol 
    make-typed-array make-u16vector make-u32vector make-u64vector make-u8vector 
    make-undefined-variable make-uniform-array make-uniform-vector 
    make-variable make-vector make-vtable make-vtable-vtable 
    make-weak-key-hash-table make-weak-value-hash-table map map-in-order 
    mask-signals max member memoized-environment memoized? memq memv merge 
    merge! micro-version min minor-version mkdir mknod mkstemp! mktime 
    module-add! module-binder module-bound? module-call-observers module-clear! 
    module-constructor module-define! module-defined? 
    module-duplicates-handlers module-duplicates-interface 
    module-ensure-local-variable! module-eval-closure module-export! 
    module-for-each module-import-interface module-kind module-local-variable 
    module-locally-bound? module-make-local-var! module-map module-modified 
    module-name module-obarray module-obarray-get-handle module-obarray-ref 
    module-obarray-remove! module-obarray-set! module-observe 
    module-observe-weak module-observer-id module-observers 
    module-public-interface module-re-export! module-ref module-remove! 
    module-replace! module-search module-set! module-symbol-binding 
    module-symbol-interned? module-symbol-local-binding 
    module-symbol-locally-interned? module-transformer module-unobserve 
    module-use! module-use-interfaces! module-uses module-variable 
    module-weak-observers module? modulo modulo-expt move->fdes 
    named-module-use! nan nan? negative? nested-define! nested-ref 
    nested-remove! nested-set! netent:addrtype netent:aliases netent:name 
    netent:net newline ngettext nice noop not ntohl ntohs null? number->string 
    number? numerator object->string object-address object-properties 
    object-property odd? open open-fdes open-file open-input-file 
    open-input-string open-io-file open-output-file open-output-string opendir 
    operator? or-map output-port? pair? parse-path passwd:dir passwd:gecos 
    passwd:gid passwd:name passwd:passwd passwd:shell passwd:uid pause peek 
    peek-char pipe pk port->fdes port-closed? port-column port-filename 
    port-for-each port-line port-mode port-revealed port-with-print-state port? 
    positive? primitive-_exit primitive-eval primitive-exit primitive-fork 
    primitive-load primitive-load-path primitive-macro? primitive-make-property 
    primitive-move->fdes primitive-property-del! primitive-property-ref 
    primitive-property-set! print-disable print-enable print-options 
    print-options-interface procedure procedure->macro 
    procedure->memoizing-macro procedure->syntax procedure-documentation 
    procedure-environment procedure-name procedure-properties 
    procedure-property procedure-source procedure-with-setter? procedure? 
    process-define-module process-duplicates process-use-modules 
    program-arguments promise? protoent:aliases protoent:name protoent:proto 
    provide provided? purify-module! putenv quit quotient raise random 
    random:exp random:hollow-sphere! random:normal random:normal-vector! 
    random:solid-sphere! random:uniform rational? rationalize read 
    read-and-eval! read-char read-disable read-enable read-hash-extend 
    read-options read-options-interface readdir readlink real-part real? 
    record-accessor record-constructor record-modifier record-predicate 
    record-type-descriptor record-type-fields record-type-name record-type? 
    record? recv! recvfrom! redirect-port regexp-exec regexp? register-modules 
    release-arbiter release-port-handle remainder remove-hook! rename-file repl 
    repl-reader reset-hook! resolve-interface resolve-module restore-signals 
    restricted-vector-sort! reverse reverse! reverse-list->string rewinddir 
    rmdir round run-asyncs run-hook s16vector s16vector->list s16vector-length 
    s16vector-ref s16vector-set! s16vector? s32vector s32vector->list 
    s32vector-length s32vector-ref s32vector-set! s32vector? s64vector 
    s64vector->list s64vector-length s64vector-ref s64vector-set! s64vector? 
    s8vector s8vector->list s8vector-length s8vector-ref s8vector-set! 
    s8vector? save-module-excursion save-stack scheme-file-suffix scm-error 
    scm-style-repl search-path seed->random-state seek select self-evaluating? 
    send sendto servent:aliases servent:name servent:port servent:proto 
    set-autoloaded! set-batch-mode?! set-car! set-cdr! 
    set-current-dynamic-state set-current-error-port set-current-input-port 
    set-current-module set-current-output-port set-defmacro-transformer! 
    set-module-binder! set-module-duplicates-handlers! 
    set-module-duplicates-interface! set-module-eval-closure! set-module-kind! 
    set-module-name! set-module-obarray! set-module-observer-id! 
    set-module-observers! set-module-public-interface! set-module-transformer! 
    set-module-uses! set-object-procedure! set-object-properties! 
    set-object-property! set-port-column! set-port-filename! set-port-line! 
    set-port-revealed! set-procedure-properties! set-procedure-property! 
    set-program-arguments set-repl-prompt! set-source-properties! 
    set-source-property! set-struct-vtable-name! set-symbol-property! 
    set-system-module! set-tm:gmtoff set-tm:hour set-tm:isdst set-tm:mday 
    set-tm:min set-tm:mon set-tm:sec set-tm:wday set-tm:yday set-tm:year 
    set-tm:zone setegid setenv seteuid setgid setgr setgrent setgroups sethost 
    sethostent sethostname setitimer setlocale setnet setnetent setpgid 
    setpriority setproto setprotoent setpw setpwent setserv setservent setsid 
    setsockopt setter setuid setvbuf shared-array-increments 
    shared-array-offset shared-array-root shutdown sigaction 
    signal-condition-variable simple-format sin sinh sleep sloppy-assoc 
    sloppy-assq sloppy-assv sloppy-member sloppy-memq sloppy-memv sockaddr:addr 
    sockaddr:fam sockaddr:flowinfo sockaddr:path sockaddr:port sockaddr:scopeid 
    socket socketpair sort sort! sort-list sort-list! sorted? source-properties 
    source-property split-c-module-name sqrt stable-sort stable-sort! stack-id 
    stack-length stack-ref stack? standard-eval-closure 
    standard-interface-eval-closure stat stat:atime stat:blksize stat:blocks 
    stat:ctime stat:dev stat:gid stat:ino stat:mode stat:mtime stat:nlink 
    stat:perms stat:rdev stat:size stat:type stat:uid status:exit-val 
    status:stop-sig status:term-sig strerror strftime string string->char-set 
    string->char-set! string->list string->number string->obarray-symbol 
    string->symbol string-any string-any-c-code string-append 
    string-append/shared string-capitalize string-capitalize! string-ci->symbol 
    string-ci< string-ci<= string-ci<=? string-ci<> string-ci<? string-ci= 
    string-ci=? string-ci> string-ci>= string-ci>=? string-ci>? string-compare 
    string-compare-ci string-concatenate string-concatenate-reverse 
    string-concatenate-reverse/shared string-concatenate/shared string-contains 
    string-contains-ci string-copy string-copy! string-count string-delete 
    string-downcase string-downcase! string-drop string-drop-right string-every 
    string-every-c-code string-fill! string-filter string-fold 
    string-fold-right string-for-each string-for-each-index string-hash 
    string-hash-ci string-index string-index-right string-join string-length 
    string-map string-map! string-null? string-pad string-pad-right 
    string-prefix-ci? string-prefix-length string-prefix-length-ci 
    string-prefix? string-ref string-replace string-reverse string-reverse! 
    string-rindex string-set! string-skip string-skip-right string-split 
    string-suffix-ci? string-suffix-length string-suffix-length-ci 
    string-suffix? string-tabulate string-take string-take-right 
    string-titlecase string-titlecase! string-tokenize string-trim 
    string-trim-both string-trim-right string-unfold string-unfold-right 
    string-upcase string-upcase! string-xcopy! string< string<= string<=? 
    string<> string<? string= string=? string> string>= string>=? string>? 
    string? strptime struct-layout struct-ref struct-set! struct-vtable 
    struct-vtable-name struct-vtable-tag struct-vtable? struct? substring 
    substring-fill! substring-move! substring-move-left! substring-move-right! 
    substring/copy substring/read-only substring/shared symbol symbol->keyword 
    symbol->string symbol-append symbol-binding symbol-bound? symbol-fref 
    symbol-fset! symbol-hash symbol-interned? symbol-pref symbol-prefix-proc 
    symbol-property symbol-property-remove! symbol-pset! symbol-set! symbol? 
    symlink sync system system* system-async system-async-mark 
    system-error-errno tan tanh tcgetpgrp tcsetpgrp textdomain thread-exited? 
    throw thunk? times tm:gmtoff tm:hour tm:isdst tm:mday tm:min tm:mon tm:sec 
    tm:wday tm:yday tm:year tm:zone tmpnam tms:clock tms:cstime tms:cutime 
    tms:stime tms:utime top-repl transform-usage-lambda transpose-array 
    trap-disable trap-enable traps truncate truncate-file try-arbiter 
    try-load-module try-module-autoload try-module-dynamic-link 
    try-module-linked try-mutex try-using-libtool-name try-using-sharlib-name 
    ttyname turn-on-debugging typed-array? tzset u16vector u16vector->list 
    u16vector-length u16vector-ref u16vector-set! u16vector? u32vector 
    u32vector->list u32vector-length u32vector-ref u32vector-set! u32vector? 
    u64vector u64vector->list u64vector-length u64vector-ref u64vector-set! 
    u64vector? u8vector u8vector->list u8vector-length u8vector-ref 
    u8vector-set! u8vector? ucs-range->char-set ucs-range->char-set! umask 
    uname uniform-array-read! uniform-array-write uniform-vector->list 
    uniform-vector-fill! uniform-vector-length uniform-vector-read! 
    uniform-vector-ref uniform-vector-set! uniform-vector-write uniform-vector? 
    unintern-symbol unlock-mutex unmask-signals unmemoize-expr unread-char 
    unread-string unsetenv unspecified? use-srfis using-readline? usleep utime 
    utsname:machine utsname:nodename utsname:release utsname:sysname 
    utsname:version valid-object-procedure? values variable-bound? variable-ref 
    variable-set! variable-set-name-hint! variable? vector vector->list 
    vector-copy vector-fill! vector-length vector-move-left! vector-move-right! 
    vector-ref vector-set! vector? version wait-condition-variable waitpid warn 
    warn-autoload-deprecation weak-key-hash-table? weak-value-hash-table? 
    with-continuation-barrier with-dynamic-state with-error-to-file 
    with-error-to-port with-error-to-string with-fluid* with-fluids* 
    with-input-from-file with-input-from-port with-input-from-string 
    with-output-to-file with-output-to-port with-output-to-string 
    with-throw-handler with-traps write write-char xsubstring yield zero?))

(defconst lyqi:scheme-guile-macros
  '(@ @@ @apply @bind @call-with-current-continuation @call-with-values @fop 
    and begin begin-deprecated case collect cond cond-expand debug-set! define 
    define-macro define-module define-option-interface define-private 
    define-public define-syntax-macro defmacro defmacro-public delay do 
    eval-case eval-set! export export-syntax false-if-exception if lambda let 
    let* letrec nil-cond or print-set! quasiquote quote re-export 
    re-export-syntax read-set! require-extension set! start-stack 
    the-environment trap-set! undefine use-modules use-syntax while with-fluids))

(provide 'lyqi-words)
