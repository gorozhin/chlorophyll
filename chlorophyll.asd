 (in-package #:cl-user)
(defpackage chlorophyll-asd (:use #:cl #:asdf))
(in-package #:chlorophyll-asd)

(defsystem #:chlorophyll
  :version "0.0.1"
  :author "Mikhail Gorozhin <m.gorozhin@gmail.com>"
  :license "Expat"
  :depends-on (
	       #:alexandria)
  :components ((:module "src"
		:components
		((:file "package")
                 (:file "color")
                 (:file "style")
                 (:file "positioning"))))
  :description "ANSI escape code library for Common Lisp"
  :long-description #.(with-open-file (stream (merge-pathnames
                                               #p"README.md"
                                               (or *load-pathname* *compile-file-pathname*))
                                              :if-does-not-exist nil
                                              :direction :input)
                        (when stream
                          (let ((seq (make-array (file-length stream)
                                                 :element-type 'character
                                                 :fill-pointer t)))
                            (setf (fill-pointer seq) (read-sequence seq stream))
                            seq)))
  :in-order-to ((test-op (test-op #:chlorophyll-test))))
