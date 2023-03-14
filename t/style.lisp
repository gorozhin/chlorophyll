(in-package #:chlorophyll-test)

(def-suite style-suite
  :description "suite to check styling capabilities")

(in-suite style-suite)

(test eloquent-constructors
  (let  ((very-peri (chlorophyll:create-rgb-color 102 103 171))
         (some-pink (chlorophyll:create-ansi-256-color 205))
         (red-ansi (chlorophyll:create-ansi-color 1))
         (red-ansi-bright (chlorophyll:create-ansi-color 9))
         (truecolor-profile (chlorophyll::create-profile (chlorophyll::new-truecolor-terminal))))

    (is (string= (chlorophyll:bold "bold text" :profile truecolor-profile)
                 (format nil "~C[1mbold text~C[0m" #\Esc #\Esc)))
    (is (string= (chlorophyll:faint "faint text" :profile truecolor-profile)
                 (format nil "~C[2mfaint text~C[0m" #\Esc #\Esc)))
    (is (string= (chlorophyll:italic "italic text" :profile truecolor-profile)
                 (format nil "~C[3mitalic text~C[0m" #\Esc #\Esc)))
    (is (string= (chlorophyll:underline "underline text" :profile truecolor-profile)
                 (format nil "~C[4munderline text~C[0m" #\Esc #\Esc)))
    (is (string= (chlorophyll:blink "blink text" :profile truecolor-profile)
                 (format nil "~C[5mblink text~C[0m" #\Esc #\Esc)))
    (is (string= (chlorophyll:invert "invert text" :profile truecolor-profile)
                 (format nil "~C[7minvert text~C[0m" #\Esc #\Esc)))
    (is (string= (chlorophyll:crossout "crossout text" :profile truecolor-profile)
                 (format nil "~C[9mcrossout text~C[0m" #\Esc #\Esc)))
    (is (string= (chlorophyll:overline "overline text" :profile truecolor-profile)
                 (format nil "~C[53moverline text~C[0m" #\Esc #\Esc)))

    (is (string= (chlorophyll:foreground very-peri "colored text" :profile truecolor-profile)
                 (format nil "~C[38;2;102;103;171mcolored text~C[0m" #\Esc #\Esc)))

    (is (string= (chlorophyll:foreground very-peri "colored text" :profile truecolor-profile)
                 (format nil "~C[38;2;102;103;171mcolored text~C[0m" #\Esc #\Esc)))

    (is (string= (chlorophyll:background some-pink "colored text" :profile truecolor-profile)
                 (format nil "~C[48;5;205mcolored text~C[0m" #\Esc #\Esc)))

    (is (string= (chlorophyll:background red-ansi "colored text" :profile truecolor-profile)
                 (format nil "~C[41mcolored text~C[0m" #\Esc #\Esc)))
    (is (string= (chlorophyll:foreground red-ansi "colored text" :profile truecolor-profile)
                 (format nil "~C[31mcolored text~C[0m" #\Esc #\Esc)))

    (is (string= (chlorophyll:background red-ansi-bright "colored text" :profile truecolor-profile)
                 (format nil "~C[101mcolored text~C[0m" #\Esc #\Esc)))
    (is (string= (chlorophyll:foreground red-ansi-bright "colored text" :profile truecolor-profile)
                 (format nil "~C[91mcolored text~C[0m" #\Esc #\Esc)))))

(test stylize
  (let* ((very-peri (chlorophyll:create-rgb-color 102 103 171))
         (hot-pink (chlorophyll:create-rgb-color 255 105 180))
         (style (chlorophyll:new-style
                 :bold t
                 :foreground hot-pink
                 :background very-peri))

         (truecolor-profile (chlorophyll::create-profile
                             (chlorophyll::new-truecolor-terminal))))

    (is (string=
         (chlorophyll:stylize style "beautiful text" :profile truecolor-profile)
         (format nil
                 "~C[1;38;2;255;105;180;48;2;102;103;171mbeautiful text~C[0m"
                 #\Esc
                 #\Esc)))))

(test degrade-colors
  (let* ((yellowish (chlorophyll:create-rgb-color 245 208 51))
         (style (chlorophyll:new-style
                        :background yellowish))
         (truecolor-profile (chlorophyll::create-profile
                             (chlorophyll::new-terminal "" "truecolor" "" nil)))
         (ansi-256-profile (chlorophyll::create-profile
                            (chlorophyll::new-terminal "xterm-256color" "" "" nil)))
         (ansi-profile (chlorophyll::create-profile
                        (chlorophyll::new-terminal "xterm-color" "" "" nil)))
         (ascii-profile (chlorophyll::create-profile
                            (chlorophyll::new-terminal "dump" "" "" nil))))

    (is (string=
         (chlorophyll:stylize style "beautiful text" :profile truecolor-profile)
         (format nil
                 "~C[48;2;245;208;51mbeautiful text~C[0m" #\Esc #\Esc)))

    (is (string=
         (chlorophyll:stylize style "beautiful text" :profile ansi-256-profile)
         (format nil
                 "~C[48;5;178mbeautiful text~C[0m" #\Esc #\Esc)))

    (is (string=
         (chlorophyll:stylize style "beautiful text" :profile ansi-profile)
         (format nil
                 "~C[103mbeautiful text~C[0m" #\Esc #\Esc)))

    (is (string=
         (chlorophyll:stylize style "beautiful text" :profile ascii-profile)
         "beautiful text"))))

