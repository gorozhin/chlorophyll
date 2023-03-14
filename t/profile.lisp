(in-package #:chlorophyll-test)

(def-suite profile-suite
  :description "suite to check profile detection")

(in-suite profile-suite)

(test no-color-aware
  (is (typep (chlorophyll::new-terminal "xterm"
                                        "xterm-256color"
                                        "term"
                                        t)
             'chlorophyll::ascii-terminal))
  (is (typep (chlorophyll::new-terminal "xterm-kitty"
                                        "truecolor"
                                        "kitty"
                                        t)
             'chlorophyll::ascii-terminal)))

(test screen-tmux-differentiation
  (is (typep (chlorophyll::new-terminal "screen-256color"
                                        "truecolor"
                                        "tmux" nil)
             'chlorophyll::truecolor-terminal))
  (is (typep (chlorophyll::new-terminal "screen" "truecolor" "" nil)
             'chlorophyll::ansi-256-terminal)))

(test terminal-detection
  (is (typep (chlorophyll::new-terminal "" "" "" nil)
             'chlorophyll::ascii-terminal))
  (is (typep (chlorophyll::new-terminal "xterm-256color" "" "" nil)
             'chlorophyll::ansi-256-terminal))
  (is (typep (chlorophyll::new-terminal "" "truecolor" "" nil)
             'chlorophyll::truecolor-terminal))
  (is (typep (chlorophyll::new-terminal "linux" "" "" nil)
             'chlorophyll::ansi-terminal))
  (is (typep (chlorophyll::new-terminal "dumb" "" "" nil)
             'chlorophyll::ascii-terminal)))

