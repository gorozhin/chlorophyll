`chlorophyll` is a ANSI escape code library for Common Lisp.

## Installation
### Quicklisp
Coming soon.

### Ultralisp
Set up [Ultralisp](https://ultralisp.org/), load `chlorophyll`:

```common-lisp
(ql:quickload "chlorophyll")
```

## Usage

```common-lisp
(let ((very-peri (chlorophyll:create-rgb-color 102 103 171))
      (hot-pink (chlorophyll:create-rgb-color 255 105 180)))
  (format t "~A~%~A~%" (chlorophyll:overline "overline text")
          (chlorophyll:stylize
           (chlorophyll:new-style
            :bold t
            :foreground hot-pink
            :background very-peri)
           "beautiful text")))
```
