(in-package #:chlorophyll)

(defclass style ()
  ((bold :type boolean
         :reader boldp
         :initform nil
         :initarg :bold
         :documentation "Defines whether text should be bold")
   (faint :type boolean
          :reader faintp
          :initform nil
          :initarg :faint
          :documentation "Defines whether text should be faint")
   (italic :type boolean
           :reader italicp
           :initform nil
           :initarg :italic
           :documentation "Defines whether text should be italic")
   (underline :type boolean
              :reader underlinep
              :initform nil
              :initarg :underline
              :documentation "Defines whether text should be underlined")
   (blink :type boolean
          :reader blinkp
          :initform nil
          :initarg :blink
          :documentation "Defines whether text should blink")
   (invert :type boolean
           :reader invertp
           :initform nil
           :initarg :invert
           :documentation "Defines whether text should have inverted foreground/background")
   (crossout :type boolean
             :reader crossoutp
             :initform nil
             :initarg :crossout)
   (overline :type boolean
             :reader overlinep
             :initform nil
             :initarg :overline
             :documentation "Defines whether text should be overlined")
   (foreground :type (or color null)
               :reader get-foreground
               :initform nil
               :initarg :foreground
               :documentation "Defines a foreground color for the text")
   (background :type (or color null)
               :reader get-background
               :initform nil
               :initarg :background
               :documentation "Defines a background color for the text"))
  (:documentation "Represents a styleset appliable for the text"))

(declaim (type (integer) +foreground+))
(define-constant +foreground+ 38
  :test #'=
  :documentation "ANSI defined constant for a background color")
(declaim (type (integer) +background+))
(define-constant +background+ 48
  :test #'=
  :documentation "ANSI defined constant for a foreground color")

(declaim (type (integer) +reset+))
(define-constant +reset+ 0
  :test #'=
  :documentation "ANSI defined constant for a reset action")
(declaim (type (integer) +bold+))
(define-constant +bold+ 1
  :test #'=
  :documentation "ANSI defined constant for bold text")
(declaim (type (integer) +faint+))
(define-constant +faint+ 2
  :test #'=
  :documentation "ANSI defined constant for faint text")
(declaim (type (integer) +italic+))
(define-constant +italic+ 3
  :test #'=
  :documentation "ANSI defined constant for italic text")
(declaim (type (integer) +underline+))
(define-constant +underline+ 4
  :test #'=
  :documentation "ANSI defined constant for an underlined text")
(declaim (type (integer) +blink+))
(define-constant +blink+ 5
  :test #'=
  :documentation "ANSI defined constant for blinking text")
(declaim (type (integer) +invert+))
(define-constant +invert+ 7
  :test #'=
  :documentation "ANSI defined constant for inverted colors")
(declaim (type (integer) +crossout+))
(define-constant +crossout+ 9
  :test #'=
  :documentation "ANSI defined constant for crossedout text")
(declaim (type (integer) +overline+))
(define-constant +overline+ 53
  :test #'=
  :documentation "ANSI defined constant for an overlined text")

(declaim (ftype (function (&key (:bold boolean)
                                (:faint boolean)
                                (:italic boolean)
                                (:underline boolean)
                                (:blink boolean)
                                (:invert boolean)
                                (:crossout boolean)
                                (:overline boolean)
                                (:foreground (or color null))
                                (:background (or color null)))
                          (values style &optional))
                new-style))
(defun new-style (&key
                    bold
                    faint
                    italic
                    underline
                    blink
                    invert
                    crossout
                    overline
                    foreground
                    background)
  "Constructor for a STYLE"
  (make-instance 'style
                 :bold bold
                 :faint faint
                 :italic italic
                 :underline underline
                 :blink blink
                 :invert invert
                 :crossout crossout
                 :overline overline
                 :foreground foreground
                 :background background))

(defmethod stylize ((s style) (str string))
  "Applies STYLE to STR returning a new string"
  (let ((compiled-styles nil))
    (macrolet
        ((define-predicate-styles (&body definitions)
           `(progn ,@(loop for (predicate value)
                             on definitions
                           by #'cddr
                           collect `(when (,predicate s)
                                      (setf compiled-styles
                                            (nconc compiled-styles
                                                   (list ,value)))))))
         (define-color-styles (&body definitions)
           `(progn ,@(loop for (color value)
                             on definitions
                           by #'cddr
                           collect
                           `(when (not (null ,color))
                              (setf compiled-styles
                                    (nconc compiled-styles
                                           (list (format nil "~A;~A"
                                                         ,value
                                                         (to-sequence ,color))))))))))
      (define-predicate-styles boldp +bold+
        faintp +faint+
        italicp +italic+
        underlinep +underline+
        blinkp +blink+
        invertp +invert+
        crossoutp +crossout+
        overlinep +overline+)
      (define-color-styles (get-foreground s) +foreground+
        (get-background s) +background+))
    (if (> (length compiled-styles) 0)
        (format nil "~C[~{~A~^;~}m~A~A[~Am"
                #\Esc
                compiled-styles
                str
                #\Esc
                +reset+)
        str)))

(defmethod to-sequence ((color no-color))
  "Converts NO-COLOR to a ANSI escape sequence color definition"
  "")

(defmethod to-sequence ((color ansi-256-color))
  "Converts ANSI-256-COLOR to a ANSI escape sequence color definition"
  (format nil "5;~A" (color-id color)))

(defmethod to-sequence ((color rgb-color))
  "Converts RGB-COLOR to a ANSI escape sequence color definition"
  (format nil "2;~A;~A;~A"
          (red color)
          (green color)
          (blue color)))

(macrolet ((define-eloquent-boolean-constructors (&rest definitions)
             `(progn ,@(loop for definition
                               in definitions
                             collect
                             (let* ((symbol-name (symbol-name definition))
                                    (interned-package-symbol (intern symbol-name
                                                                     '#:chlorophyll))
                                    (interned-keyword-symbol (intern symbol-name
                                                                     '#:keyword)))
                               `(progn
                                  (declaim (ftype (function (string)
                                                            (values string &optional))
                                                  ,interned-package-symbol))
                                  (defun ,interned-package-symbol
                                      (str)
                                    (let ((style
                                            (new-style ,interned-keyword-symbol
                                                       t)))
                                      (stylize style str)))
                                  (setf (documentation ',interned-package-symbol 'function)
                                        (format nil
                                                "Eloquent constructor setting ~A"
                                                ',interned-package-symbol))
                                  (export ',interned-package-symbol '#:chlorophyll))))))
           (define-eloquent-color-constructors (&rest definitions)             
             `(progn ,@(loop for definition
                               in definitions
                             collect
                             (let* ((symbol-name (symbol-name definition))
                                    (interned-package-symbol (intern symbol-name
                                                                     '#:chlorophyll))
                                    (interned-keyword-symbol (intern symbol-name
                                                                     '#:keyword)))
                               `(progn
                                  (declaim (ftype (function (color string)
                                                            (values string &optional))
                                                  ,interned-package-symbol))
                                  (defun ,interned-package-symbol
                                      (color str)
                                    (let ((style
                                            (new-style ,interned-keyword-symbol
                                                       color)))
                                      (stylize style str)))
                                  (setf (documentation ',interned-package-symbol 'function)
                                        (format nil
                                                "Eloquent constructor setting ~A"
                                                ',interned-package-symbol))
                                  (export ',interned-package-symbol '#:chlorophyll)))))))

  (define-eloquent-boolean-constructors
    bold
    faint
    italic
    underline
    blink
    invert
    crossout
    overline)

  (define-eloquent-color-constructors foreground
    background))

