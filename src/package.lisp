(in-package #:cl-user)
(uiop:define-package #:chlorophyll
     (:use #:cl #:alexandria)
  (:export #:create-ansi-256-color
           #:create-rgb-color
           #:create-no-color
           #:new-style
           #:stylize))
(in-package #:chlorophyll)

