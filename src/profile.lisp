(in-package #:chlorophyll)

(defclass profile ()
  ((terminal :type terminal
             :reader terminal
             :initarg :terminal
             :documentation "Terminal features"))
  (:documentation "Chlorophyll profile"))

(declaim (ftype (function (terminal)
                          (values profile &optional))
                create-new-profile))
(defun create-profile (terminal)
  (make-instance 'profile :terminal terminal))

(defclass terminal () ()
  (:documentation "Base class representing terminal type, not to be used directly"))

(defmethod supports-effects-p ((terminal terminal))
  nil)

(defclass ascii-terminal (terminal)
  ()
  (:documentation "Represents dumb terminal, no colors, no effects."))

(declaim (ftype (function ()
                          (values ascii-terminal &optional))
                new-ascii-terminal))
(defun new-ascii-terminal ()
  (make-instance 'ascii-terminal))

(defmethod supports-effects-p ((terminal ascii-terminal))
  nil)

(defclass ansi-terminal (terminal)
  ()
  (:documentation "Represents 3(4) bit color terminal"))

(declaim (ftype (function ()
                          (values ansi-terminal &optional))
                new-ansi-terminal))
(defun new-ansi-terminal ()
  (make-instance 'ansi-terminal))

(defmethod supports-effects-p ((terminal ansi-terminal))
  t)

(defclass ansi-256-terminal (terminal)
  ()
  (:documentation "Represents 8 bit color terminal"))

(defun new-ansi-256-terminal ()
  (make-instance 'ansi-256-terminal))

(defmethod supports-effects-p ((terminal ansi-256-terminal))
  t)

(defclass truecolor-terminal (terminal)
  ()
  (:documentation "Representing 8 bit color terminal"))

(declaim (ftype (function ()
                          (values truecolor-terminal &optional))
                new-truecolor-terminal))
(defun new-truecolor-terminal ()
  (make-instance 'truecolor-terminal))

(defmethod supports-effects-p ((terminal truecolor-terminal))
  t)

(declaim (ftype (function (string
                           string
                           string
                           boolean)
                          (values terminal &optional))
                new-terminal))
(labels ((string-inside-list-p (s l)
           (remove-if-not #'(lambda (x) (string= x s)) l))
         (string-contains-inside-list-p (s l)
           (remove-if-not #'(lambda (x)
                              (if (= (length s) 0)
                                  nil
                                  (search x s)))
                          l)))
  
  (let (;; fullnames, should be checked with string-inside-list-p
        (truecolor-colors-fullnames '("24bit" "truecolor"))
        (truecolor-terms-fullnames '("xterm-kitty"))
        (ansi-256-colors-fullnames '("yes" "true"))
        (ansi-terms-fullnames '("linux"))
        ;; substrings, should be checked with string-contains-inside-list-p
        (ansi-terms-substrings '("color" "ansi"))
        (ansi-256-terms-substrings '("256color")))
    
    (defun new-terminal (term color-term term-program no-color-p)
      (let* (;; color predicates
             (truecolor-p (or (string-inside-list-p color-term truecolor-colors-fullnames)
                              (string-inside-list-p term truecolor-terms-fullnames)))
             (ansi-256-p (or (string-inside-list-p color-term ansi-256-colors-fullnames)
                             (string-contains-inside-list-p term ansi-256-terms-substrings)))
             (ansi-p (or (string-contains-inside-list-p term ansi-terms-substrings)
                         (string-inside-list-p term ansi-terms-fullnames))))

        (flet ((truecolor-selector ()
                 ;; tmux supports 24bit color, screen deos not
                 (if (uiop:string-prefix-p "screen" term)
                     (if (string= term-program "tmux")
                         (new-truecolor-terminal)
                         (new-ansi-256-terminal))
                     (new-truecolor-terminal))))
          (cond
            (no-color-p (new-ascii-terminal))
            (truecolor-p (truecolor-selector))
            (ansi-256-p (new-ansi-256-terminal))
            (ansi-p (new-ansi-terminal))
            (t (new-ascii-terminal))))))))

(defun new-profile-from-env ()
  (flet ((downstring-getenv (x)
           (string-downcase (uiop:getenv x))))
    (let ((term (downstring-getenv "TERM"))
          (color-term (downstring-getenv "COLORTERM"))
          (term-program (downstring-getenv "TERM_PROGRAM"))
          (no-color (uiop:getenv "NO_COLOR")))
      (create-profile (new-terminal term
                                             color-term
                                             term-program
                                             (not (alexandria:emptyp no-color)))))))

(let ((profile nil))
  (defun new-profile-from-env-momoised ()
    (when (null profile)
      (setf profile (new-profile-from-env)))
    profile))

