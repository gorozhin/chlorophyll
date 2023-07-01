(in-package #:chlorophyll)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass color-mode () ()
    (:documentation "defines mode in which color is used, not to be used directly"))
  (defclass foreground-mode (color-mode) ()
    (:documentation "defines foreground color mode"))
  (defclass background-mode (color-mode) ()
    (:documentation "defines background color mode"))
  (defmethod equal-mode ((x foreground-mode) (y t))
    nil)
  (defmethod equal-mode ((x t) (y foreground-mode))
    nil)
  (defmethod equal-mode ((x foreground-mode) (y foreground-mode))
    t)
  (defmethod equal-mode ((x background-mode) (y t))
    nil)
  (defmethod equal-mode ((x t) (y background-mode))
    nil)
  (defmethod equal-mode ((x background-mode) (y background-mode))
    t)
  (define-constant +foreground-mode+ (make-instance 'foreground-mode)
    :test #'equal-mode)
  (define-constant +background-mode+ (make-instance 'background-mode)
    :test #'equal-mode)
  (defmethod make-load-form ((m color-mode) &optional env)
    (declare (ignore env))
    (make-load-form-saving-slots m)))

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

;; ansi
(declaim (type (integer) +foreground-ansi-addup+))
(define-constant +foreground-ansi-addup+ 30
  :test #'=
  :documentation "addup constant for a background color")
(declaim (type (integer) +background-ansi-addup+))
(define-constant +background-ansi-addup+ 40
  :test #'=
  :documentation "addup constant for a foreground color")
;; extended ansi
(declaim (type (integer) +foreground-extended-ansi-addup+))
(define-constant +foreground-extended-ansi-addup+ 82
  :test #'=
  :documentation "addup constant for a background color")
(declaim (type (integer) +background-extended-ansi-addup+))
(define-constant +background-extended-ansi-addup+ 92
  :test #'=
  :documentation "addup constant for a foreground color")
;; ansi-256 and truecolor
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

(defmethod stylize ((s style) (str string) &key (profile *profile*))
  "Applies STYLE to STR returning a new string"
  (let* ((compiled-styles nil)
         (terminal (terminal profile)))
    (macrolet
        ((define-predicate-styles (&body definitions)
           `(progn ,@(loop for (predicate value)
                             on definitions
                           by #'cddr
                           collect `(when (and (supports-effects-p terminal)
                                               (,predicate s))
                                      (setf compiled-styles
                                            (nconc compiled-styles
                                                   (list ,value)))))))
         (define-color-styles (&body definitions)
           `(progn ,@(loop for (color value)
                             on definitions
                           by #'cddr
                           collect
                           `(when (not (null ,color))
                              (let ((sequence (to-sequence (adapt ,color terminal)
                                                              ,value)))
                                (when (not (string= sequence ""))
                                  (setf compiled-styles
                                    (nconc compiled-styles
                                           (list sequence))))))))))
      (define-predicate-styles boldp +bold+
        faintp +faint+
        italicp +italic+
        underlinep +underline+
        blinkp +blink+
        invertp +invert+
        crossoutp +crossout+
        overlinep +overline+)
      (define-color-styles
        (get-foreground s) +foreground-mode+
        (get-background s) +background-mode+))

    (if (> (length compiled-styles) 0)
        (format nil "~C[~{~A~^;~}m~A~A[~Am"
                #\Esc
                compiled-styles
                str
                #\Esc
                +reset+)
        str)))

(defmethod to-sequence ((color no-color) (mode color-mode))
  "Converts NO-COLOR to a ANSI escape sequence color definition"
  "")

(macrolet ((define-to-sequence (mode extended-addup addup)
             `(defmethod to-sequence ((color ansi-color) (mode ,mode))
                "Converts ANSI-COLOR to an escape sequence color definition"
                (format nil "~A" (+ (color-id color)
                                    (if (extended-ansi-p color)
                                        ,extended-addup
                                        ,addup))))))
  (define-to-sequence foreground-mode
    +foreground-extended-ansi-addup+
    +foreground-ansi-addup+)
  (define-to-sequence background-mode
    +background-extended-ansi-addup+
    +background-ansi-addup+))

(macrolet ((define-to-sequence (color-type mode code)
             `(defmethod to-sequence ((color ,color-type) (mode ,mode))
                ,(format nil "Converts ~A to an escape sequence color definition" (symbol-name color-type))
                (format nil "~A;~A" ,code (color-seq color)))))

  (flet ((color-seq (color)
           (format nil "2;~A;~A;~A"
                   (red color)
                   (green color)
                   (blue color))))
    (define-to-sequence rgb-color background-mode +background+)
    (define-to-sequence rgb-color foreground-mode +foreground+))

  (flet ((color-seq (color) (format nil "5;~A" (color-id color))))
    (define-to-sequence ansi-256-color background-mode +background+)
    (define-to-sequence ansi-256-color foreground-mode +foreground+)))

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
                                  (declaim (ftype (function (string &key (:profile profile))
                                                            (values string &optional))
                                                  ,interned-package-symbol))
                                  (defun ,interned-package-symbol
                                      (str &key (profile *profile*))
                                    (let ((style
                                            (new-style ,interned-keyword-symbol
                                                       t)))
                                      (stylize style str :profile profile)))
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
                                  (declaim (ftype (function (color string &key (:profile profile))
                                                            (values string &optional))
                                                  ,interned-package-symbol))
                                  (defun ,interned-package-symbol
                                      (color str &key (profile *profile*))
                                    (let ((style
                                            (new-style ,interned-keyword-symbol
                                                       color)))
                                      (stylize style str :profile profile)))
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

