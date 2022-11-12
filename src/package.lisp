(in-package #:cl-user)
(uiop:define-package #:chlorophyll
     (:use #:cl #:alexandria)
  (:export ;; color symbols
           #:create-ansi-256-color
           #:create-rgb-color
           #:create-no-color
           ;; styling functions
           ;; exports additional symbols inside a macro
           #:new-style
           #:stylize
           ;; positioning
           ;; exports additional symbols inside a macro
           #:move-cursor
           #:save-cursor-position
           #:restore-cursor-position))
(in-package #:chlorophyll)

