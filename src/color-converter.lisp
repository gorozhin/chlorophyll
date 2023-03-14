(in-package #:chlorophyll)

(defmethod to-no-color ((color color)))
(defmethod to-rgb-color ((color color))
  (create-rgb-color 0 0 0))
(defmethod to-ansi-256-color ((color color))
  (create-ansi-256-color +ansi-black+))
(defmethod to-ansi-color ((color color))
  (create-ansi-color +ansi-black+))

(flet ((assoc-color (cid colors)
         (cadr (assoc cid colors :test #'=)))
       (normalize (n) (nth n *ansi-256-color-cube-normalized-lengths*)))

  (defmethod to-rgb-color ((color ansi-color))
    (assoc-color (color-id color) *ansi-colors*))

  (defmethod to-rgb-color ((color ansi-256-color))
    (let ((cid (color-id color)))
      (cond ((<= +grey-codes-start+ cid +grey-codes-end+)
             (assoc-color cid *shades-of-grey*))
            ((< cid +ansi-256-start+)
             (assoc-color cid *ansi-colors*))
            (t (let ((base-color (- cid +ansi-256-start+)))
                 (multiple-value-bind (reminder b-factor)
                     (floor base-color +ansi-256-color-cube-dimension+)
                   (multiple-value-bind (r-factor g-factor)
                       (floor reminder +ansi-256-color-cube-dimension+)
                     (create-rgb-color (normalize r-factor)
                                       (normalize g-factor)
                                       (normalize b-factor))))))))))
(defmethod to-rgb-color ((color rgb-color)) color)

(flet ((diff-square (x y) (expt (- x y) 2)))
  (defmethod distance ((x-color rgb-color) (y-color rgb-color))
    ;;; default squares
    (expt (+ (diff-square (red x-color) (red y-color))
             (diff-square (green x-color) (green y-color))
             (diff-square (blue x-color) (blue y-color)))
          1/2)))

(defmethod to-ansi-256-color ((color ansi-color))
  (create-ansi-256-color (color-id color)))

(defmethod to-ansi-256-color ((color ansi-256-color))
  color)

(defmethod to-ansi-color ((color ansi-color))
  (create-ansi-color (color-id color)))

(labels ((make-distance-calculator (original-point)
           #'(lambda (x y)
               (let ((x-distance (distance original-point (cadr x)))
                     (y-distance (distance original-point (cadr y))))
                 (if (< x-distance y-distance) x y))))
         (to-base-5 (x)
           (or
            (loop for i in (cdr *ansi-256-color-cube-normalized-lengths*)
                  for j from 0
                  if (< x i) do (return j))
            +ansi-256-normalization-factor+))
         (rgb-to-ansi-256-cube (color)
           (let* ((color-id (+ +ansi-256-start+
                               (* #.(expt +ansi-256-color-cube-dimension+ 2)
                                  (to-base-5 (red color)))
                               (* +ansi-256-color-cube-dimension+
                                  (to-base-5 (green color)))
                               (to-base-5 (blue color))))
                  (guessed-color (create-ansi-256-color color-id)))
             (values guessed-color color-id (to-rgb-color guessed-color)))))
  (defmethod to-ansi-256-color ((color rgb-color))
    (multiple-value-bind (guessed-color guessed-color-id guessed-color-rgb)
        (rgb-to-ansi-256-cube color)
      (declare (ignore guessed-color))
      (create-ansi-256-color
       (car (reduce (make-distance-calculator color)
                    ;; *predefined-colors*
                    *ansi-colors*
                    :initial-value (list guessed-color-id
                                         guessed-color-rgb))))))
  
  (defmethod to-ansi-color ((color rgb-color))
    (create-ansi-color (car (reduce (make-distance-calculator color)
            (cdr *ansi-colors*)
            :initial-value (car *ansi-colors*)))))

  (defmethod to-ansi-color ((color ansi-256-color))
    (let ((cid (color-id color)))
      (if (< cid +ansi-256-start+)
          (create-ansi-color cid)
          (to-ansi-color (to-rgb-color color))))))

;; nocolor conversions
(defmethod adapt ((color no-color) (terminal terminal))
  color)

(defmethod adapt ((color color) (terminal ascii-terminal))
  (create-no-color))

;; rgb-color
(defmethod adapt ((color rgb-color) (terminal truecolor-terminal))
  color)

(defmethod adapt ((color rgb-color) (terminal ansi-256-terminal))
  (to-ansi-256-color color))

(defmethod adapt ((color rgb-color) (terminal ansi-terminal))
  (to-ansi-color color))

;; ansi-256-color
(defmethod adapt ((color ansi-256-color) (terminal truecolor-terminal))
  color)

(defmethod adapt ((color ansi-256-color) (terminal ansi-256-terminal))
  color)

(defmethod adapt ((color ansi-256-color) (terminal ansi-terminal))
  (to-ansi-color color))

;; ansi-color
(defmethod adapt ((color ansi-color) (terminal truecolor-terminal))
  (to-ansi-color color))

(defmethod adapt ((color ansi-color) (terminal ansi-256-terminal))
  (to-ansi-256-color color))

(defmethod adapt ((color ansi-color) (terminal ansi-terminal))
  color)

