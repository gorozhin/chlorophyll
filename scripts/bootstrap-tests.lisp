(handler-bind (#+asdf3.2 (asdf:bad-system-name (function MUFFLE-WARNING)))
  (handler-case (ql:quickload "chlorophyll-test" :force t)
    (error (a)
      (format t "caught error ~s~%~a~%" a a)
      (uiop:quit 2))))

(asdf:test-system "chlorophyll")
(uiop:quit 0)
