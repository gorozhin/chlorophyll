(in-package #:chlorophyll)

(defclass color () ()
  (:documentation "Base class representing color, not to be used directly"))

(defclass no-color (color) ()
  (:documentation "NOOP color class, not representing any real color"))

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

(declaim (ftype (function () (values no-color &optional))
                create-no-color))
(defun create-no-color ()
  "Constructor for an NO-COLOR"
  (make-instance 'no-color))

