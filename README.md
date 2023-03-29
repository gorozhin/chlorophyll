`chlorophyll` is a ANSI escape code library for Common Lisp.

## Installation
### Quicklisp
Set up [Quicklisp](https://www.quicklisp.org/beta/), load `chlorophyll`:

```common-lisp
(ql:quickload "chlorophyll")
```

### Ultralisp
Set up [Quicklisp](https://www.quicklisp.org/beta/) and [Ultralisp](https://ultralisp.org/), load `chlorophyll`:

```common-lisp
(ql:quickload "chlorophyll")
```

## Usage

```common-lisp
(let ((very-peri (chlorophyll:create-rgb-color 102 103 171))
      (hot-pink (chlorophyll:create-rgb-color 255 105 180)))
  (format t "~A~%~A~%" (chlorophyll:bold "overline text")
          (chlorophyll:stylize
           (chlorophyll:new-style
            :bold t
            :foreground hot-pink
            :background very-peri)
           "beautiful text")))
```
