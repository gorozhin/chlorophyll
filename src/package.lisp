(in-package #:cl-user)
(uiop:define-package #:chlorophyll
     (:use #:cl #:alexandria)
  (:export ;; color symbols
           #:create-ansi-color
           #:create-ansi-256-color
           #:create-rgb-color
           #:create-no-color
           ;; styling functions
           ;; exports additional symbols inside macro
           #:new-style
           #:stylize
           ;; positioning
           ;; exports additional symbols inside a macro
           #:move-cursor
           #:save-cursor-position
           #:restore-cursor-position
           ;; profile
           #:new-profile-from-env
           #:new-terminal
           #:create-profile
           #:new-ascii-terminal
           #:new-ansi-terminal
           #:new-ansi-256-terminal
           #:new-truecolor-terminal))
(in-package #:chlorophyll)

