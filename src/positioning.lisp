(in-package #:chlorophyll)

(declaim (ftype (function (integer integer)
                          null)
                move-cursor))
(defun move-cursor (row column)
  "Moves cursor to desired position"
  (format t "~C[~A;~AH" #\Esc row column))

(declaim (ftype (function ()
                          null)
                save-cursor-position))
(defun save-cursor-position ()
  "Saves current cursor position"
  (format t "~C[s" #\Esc))

(declaim (ftype (function ()
                          null)
                restore-cursor-position))
(defun restore-cursor-position ()
  "Restores the cursor position"
  (format t "~C[u" #\Esc))

(macrolet ((define-cursor-positioning-functions (&body definitions)
             `(progn ,@(loop for (function-name control-sequence documentation)
                               on definitions
                             by #'cdddr
                             collect `(progn
                                        (declaim (ftype (function (&optional integer)
                                                                  null)
                                                        ,function-name))
                                        (defun ,function-name (&optional (n 1))
                                          (format t "~C[~A~A" #\Esc n ,control-sequence))
                                        (setf (documentation ',function-name 'function)
                                              ,documentation)
                                        (export ',function-name '#:chlorophyll))))))
  
  (define-cursor-positioning-functions
    cursor-up            "A" "Moves cursor N rows up"
    cursor-down          "B" "Moves cursor N rows down"
    cursor-forward       "C" "Moves cursor N columns forward"
    cursor-backward      "D" "Moves cursor N columns backward"
    cursor-next-line     "E" "Moves cursor to beginning of the line n lines up"
    cursor-previous-line "F" "Moves cursor to beginning of the line n lines down"))

