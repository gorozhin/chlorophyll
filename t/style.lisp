(in-package #:chlorophyll-test)

(def-suite style-suite
  :description "suite to check styling capabilities")

(in-suite style-suite)

(test eloquent-constructors
  (let  ((very-peri (chlorophyll:create-rgb-color 102 103 171))
         (some-pink (chlorophyll:create-ansi-256-color 205)))
    (is (string= (chlorophyll:bold "bold text")
                 (format nil "~C[1mbold text~C[0m" #\Esc #\Esc)))
    (is (string= (chlorophyll:faint "faint text")
                 (format nil "~C[2mfaint text~C[0m" #\Esc #\Esc)))
    (is (string= (chlorophyll:italic "italic text")
                 (format nil "~C[3mitalic text~C[0m" #\Esc #\Esc)))
    (is (string= (chlorophyll:underline "underline text")
                 (format nil "~C[4munderline text~C[0m" #\Esc #\Esc)))
    (is (string= (chlorophyll:blink "blink text")
                 (format nil "~C[5mblink text~C[0m" #\Esc #\Esc)))
    (is (string= (chlorophyll:invert "invert text")
                 (format nil "~C[7minvert text~C[0m" #\Esc #\Esc)))
    (is (string= (chlorophyll:crossout "crossout text")
                 (format nil "~C[9mcrossout text~C[0m" #\Esc #\Esc)))
    (is (string= (chlorophyll:overline "overline text")
                 (format nil "~C[53moverline text~C[0m" #\Esc #\Esc)))

    (is (string= (chlorophyll:foreground very-peri "colored text")
                 (format nil "~C[38;2;102;103;171mcolored text~C[0m" #\Esc #\Esc)))
    
    (is (string= (chlorophyll:foreground very-peri "colored text")
                 (format nil "~C[38;2;102;103;171mcolored text~C[0m" #\Esc #\Esc)))

    (is (string= (chlorophyll:background some-pink "colored text")
                 (format nil "~C[48;5;205mcolored text~C[0m" #\Esc #\Esc)))))

(test stylize
  (let ((very-peri (chlorophyll:create-rgb-color 102 103 171))
        (hot-pink (chlorophyll:create-rgb-color 255 105 180)))

    (is (string=
         (chlorophyll:stylize
          (chlorophyll:new-style
           :bold t
           :foreground hot-pink
           :background very-peri)
          "beautiful text")
         (format nil
                 "~C[1;38;2;255;105;180;48;2;102;103;171mbeautiful text~C[0m" #\Esc #\Esc)))))

