(in-package #:chlorophyll)

(defclass color () ()
  (:documentation "Base class representing color, not to be used directly"))

(defclass no-color (color) ()
  (:documentation "NOOP color class, not representing any real color"))

(defclass rgb-color (color)
  ((red :type (integer 0 255)
        :initarg :red
        :accessor red
        :documentation "Amount of RED in color")
   (green :type (integer 0 255)
          :initarg :green
          :accessor green
          :documentation "Amount of GREEN in color")
   (blue :type (integer 0 255)
         :initarg :blue
         :accessor blue
         :documentation "Amount of BLUE in color"))
  (:documentation "Represents a 24-bit RGB color"))

(declaim (ftype (function ((integer 0 255)
                           (integer 0 255)
                           (integer 0 255))
                          (values rgb-color &optional))
                create-rgb-color))
(defun create-rgb-color (red green blue)
  "Constructor for an RGB-COLOR"
  (make-instance 'rgb-color
                 :red red
                 :green green
                 :blue blue))

(define-constant +grey-initial-offset+ 8
  :test #'=)
(define-constant +grey-step+ 10
  :test #'=)
(define-constant +grey-codes-start+ 232
  :test #'=)
(define-constant +grey-codes-end+ 255
  :test #'=)
(define-constant +ansi-256-start+ 16
  :test #'=)
(define-constant +ansi-256-color-cube-dimension+ 6
  :test #'=)
(define-constant +ansi-black+ 0 
  :test #'=)
(define-constant +ansi-256-normalization-factor+ (- +ansi-256-color-cube-dimension+ 1)
  :test #'=)
(defparameter *ansi-256-color-cube-normalized-lengths* (list 0 95 135 175 215 255))

(defparameter *ansi-colors*
      (list
       (list 0 (create-rgb-color 0 0 0))
       (list 1 (create-rgb-color 128 0 0))
       (list 2 (create-rgb-color 0 128 0))
       (list 3 (create-rgb-color 128 128 0))
       (list 4 (create-rgb-color 0 0 128))
       (list 5 (create-rgb-color 128 0 128))
       (list 6 (create-rgb-color 0 128 128))
       (list 7 (create-rgb-color 192 192 192))
       (list 8 (create-rgb-color 128 128 128))
       (list 9 (create-rgb-color 255 0 0))
       (list 10 (create-rgb-color 0 255 0))
       (list 11 (create-rgb-color 255 255 0))
       (list 12 (create-rgb-color 0 0 255))
       (list 13 (create-rgb-color 255 0 255))
       (list 14 (create-rgb-color 0 255 255))
       (list 15 (create-rgb-color 255 255 255))))

(defparameter *shades-of-grey*
      (loop for i from +grey-codes-start+ to +grey-codes-end+
            for j from 0 by +grey-step+
            collect (let ((grey-color (+ +grey-initial-offset+ j)))
                      (list i (create-rgb-color grey-color
                                                grey-color
                                                grey-color)))))

(defparameter *predefined-colors* (append *ansi-colors* *shades-of-grey*))

(defclass ansi-color
    (color)
  ((color-id :type (integer 0 15)
             :accessor color-id
             :initarg :color-id
             :documentation
             "Position in a predefined set of colors"))
  (:documentation "Represents a color from predefined ANSI set of colors"))

(declaim (ftype (function ((integer 0 15))
                          (values ansi-color &optional))
                create-ansi-color))
(defun create-ansi-color (color-id)
  "Constructor for an ANSI-COLOR"
  (make-instance 'ansi-color :color-id color-id))

(define-constant +extended-ansi-start+ 8
  :test #'=
  :documentation "Code point representing start of extended ANSI set of colors")

(defmethod extended-ansi-p ((color ansi-color))
  (>= (color-id color) 8))

(defclass ansi-256-color
    (color)
  ((color-id :type (integer 0 255)
             :accessor color-id
             :initarg :color-id
             :documentation
             "Position in a predefined set of colors"))
  (:documentation "Represents a color from predefined ANSI set of colors"))

(declaim (ftype (function ((integer 0 255))
                          (values ansi-256-color &optional))
                create-ansi-256-color))
(defun create-ansi-256-color (color-id)
  "Constructor for an ANSI-256-COLOR"
  (make-instance 'ansi-256-color :color-id color-id))

(declaim (ftype (function () (values no-color &optional))
                create-no-color))
(defun create-no-color ()
  "Constructor for an NO-COLOR"
  (make-instance 'no-color))

