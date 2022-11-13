(in-package #:chlorophyll)

(declaim (ftype (function (integer integer &key (:stream stream))
                          null)
                move-cursor))
(defun move-cursor (row column &key (stream *standard-output*))
  "Moves cursor to desired position"
  (format stream "~C[~A;~AH" #\Esc row column))

(declaim (ftype (function (&key (:stream stream))
                          null)
                save-cursor-position))
(defun save-cursor-position (&key (stream *standard-output*))
  "Saves current cursor position"
  (format stream "~C[s" #\Esc))

(declaim (ftype (function (&key (:stream stream))
                          null)
                restore-cursor-position))
(defun restore-cursor-position (&key (stream *standard-output*))
  "Restores the cursor position"
  (format stream "~C[u" #\Esc))

(macrolet ((define-cursor-positioning-functions (&body definitions)
             `(progn ,@(loop for (function-name control-sequence documentation)
                               on definitions
                             by #'cdddr
                             collect `(progn
                                        (declaim (ftype (function (&key (:n integer) (:stream stream))
                                                                  null)
                                                        ,function-name))
                                        (defun ,function-name (&key (n 1) (stream *standard-output*))
                                          (format stream "~C[~A~A" #\Esc n ,control-sequence))
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

