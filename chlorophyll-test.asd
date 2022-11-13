(in-package #:cl-user)
(uiop:define-package #:chlorophyll-test-asd
    (:use #:cl #:asdf #:uiop))
(in-package #:chlorophyll-test-asd)

(defun run-tests ()
  (let ((suites
	  (list (intern* 'style-suite '#:chlorophyll-test)
                (intern* 'positioning-suite '#:chlorophyll-test))))
    (when (not (reduce
		#'(lambda (x y) (and x y))
		(loop for suite in suites
		      collecting (symbol-call
				  :fiveam '#:run! suite))
		:initial-value t))
      (and (uiop:getenvp "CHLOROPHYLL_EXIT_ON_FAIL")
	   (uiop:quit 125)))))

(defsystem #:chlorophyll-test
  :version "0.0.1"
  :author "Mikhail Gorozhin <m.gorozhin@gmail.com>"
  :license "Expat"
  :depends-on (#:chlorophyll
	       #:fiveam
	       #:alexandria)
  :components ((:module "t"
		:components
		((:file "package")
                 (:file "style")
                 (:file "positioning"))))
  :perform (test-op (o c)
		    (run-tests)))

