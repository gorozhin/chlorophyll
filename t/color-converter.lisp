(in-package #:chlorophyll-test)

(def-suite color-converter-suite
  :description "suite to check color converters")

(in-suite color-converter-suite)

(test color-degrade-truecolor
  (let* ((color (chlorophyll::create-rgb-color 40 40 110))

         (truecolor-terminal (chlorophyll::new-truecolor-terminal))
         (ansi-256-terminal (chlorophyll::new-ansi-256-terminal))
         (ansi-terminal (chlorophyll::new-ansi-terminal))
         (ascii-terminal (chlorophyll::new-ascii-terminal))

         (truecolor (chlorophyll::adapt color truecolor-terminal))
         (ansi-256-color (chlorophyll::adapt color ansi-256-terminal))
         (ansi-color (chlorophyll::adapt color ansi-terminal))
         (no-color (chlorophyll::adapt color ascii-terminal)))

    (is (and (typep truecolor 'chlorophyll::rgb-color)
             (= (chlorophyll::red truecolor) (chlorophyll::red color))
             (= (chlorophyll::green truecolor) (chlorophyll::green color))
             (= (chlorophyll::blue truecolor) (chlorophyll::blue color))))
    (is (and
         (typep ansi-256-color 'chlorophyll::ansi-256-color)
         (= (chlorophyll::color-id ansi-256-color) 17)))
    (is (and
         (typep ansi-color 'chlorophyll::ansi-color)
         (= (chlorophyll::color-id ansi-color) 4)))
    (is (typep no-color 'chlorophyll::no-color))))

(test color-degrade-ansi-256-color
  (let* ((color (chlorophyll::create-ansi-256-color 17))

         (truecolor-terminal (chlorophyll::new-truecolor-terminal))
         (ansi-256-terminal (chlorophyll::new-ansi-256-terminal))
         (ansi-terminal (chlorophyll::new-ansi-terminal))
         (ascii-terminal (chlorophyll::new-ascii-terminal))
         
         (truecolor (chlorophyll::adapt color truecolor-terminal))
         (ansi-256-color (chlorophyll::adapt color ansi-256-terminal))
         (ansi-color (chlorophyll::adapt color ansi-terminal))
         (no-color (chlorophyll::adapt color ascii-terminal)))

    (is (and (typep truecolor 'chlorophyll::ansi-256-color)
             (= (chlorophyll::color-id truecolor) 17)))
    (is (= (chlorophyll::color-id ansi-256-color) 17))
    (is (= (chlorophyll::color-id ansi-color) 4))
    (is (typep no-color 'chlorophyll::no-color))))

(test color-degrade-ansi-color
  (let* ((color (chlorophyll::create-ansi-color 4))

         (truecolor-terminal (chlorophyll::new-truecolor-terminal))
         (ansi-256-terminal (chlorophyll::new-ansi-256-terminal))
         (ansi-terminal (chlorophyll::new-ansi-terminal))
         (ascii-terminal (chlorophyll::new-ascii-terminal))

         (truecolor (chlorophyll::adapt color truecolor-terminal))
         (ansi-256-color (chlorophyll::adapt color ansi-256-terminal))
         (ansi-color (chlorophyll::adapt color ansi-terminal))
         (no-color (chlorophyll::adapt color ascii-terminal)))

    (is (and (typep truecolor 'chlorophyll::ansi-color)
             (= (chlorophyll::color-id truecolor) 4)))
    (is (= (chlorophyll::color-id ansi-256-color) 4))
    (is (= (chlorophyll::color-id ansi-color) 4))
    (is (typep no-color 'chlorophyll::no-color))))

(test color-degrade-ansi-color
  (let* ((color (chlorophyll::create-no-color))

         (truecolor-terminal (chlorophyll::new-truecolor-terminal))
         (ansi-256-terminal (chlorophyll::new-ansi-256-terminal))
         (ansi-terminal (chlorophyll::new-ansi-terminal))
         (ascii-terminal (chlorophyll::new-ascii-terminal))

         (truecolor (chlorophyll::adapt color truecolor-terminal))
         (ansi-256-color (chlorophyll::adapt color ansi-256-terminal))
         (ansi-color (chlorophyll::adapt color ansi-terminal))
         (no-color (chlorophyll::adapt color ascii-terminal)))

    (is (typep truecolor 'chlorophyll::no-color))
    (is (typep ansi-256-color 'chlorophyll::no-color))
    (is (typep ansi-color 'chlorophyll::no-color))
    (is (typep no-color 'chlorophyll::no-color))))

