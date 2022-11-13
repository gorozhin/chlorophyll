(in-package #:chlorophyll-test)

(def-suite positioning-suite
  :description "suite to check positioning capabilities")

(in-suite positioning-suite)

(test positioning-functions
  (macrolet ((with-s-as-output (s &body body)
               (with-gensyms (str)
                 `(let ((,str (make-array 0
                                          :element-type 'character
                                          :fill-pointer 0
                                          :adjustable t)))
                    (with-output-to-string (,s ,str)
                      ,@body
                      ,str)))))

    (let ((row 5)
          (col 5))
      (is (string= (with-s-as-output s
                     (chlorophyll:move-cursor row col :stream s))
                   (format nil "~C[5;5H" #\Esc)))
      (is (string= (with-s-as-output s
                     (chlorophyll:save-cursor-position :stream s))
                   (format nil "~C[s" #\Esc)))
      (is (string= (with-s-as-output s
                     (chlorophyll:restore-cursor-position :stream s))
                   (format nil "~C[u" #\Esc)))

      (is (string= (with-s-as-output s
                     (chlorophyll:cursor-up :stream s))
                   (format nil "~C[1A" #\Esc)))
      (is (string= (with-s-as-output s
                     (chlorophyll:cursor-down :n 5 :stream s))
                   (format nil "~C[5B" #\Esc))))))

