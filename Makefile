.PHONY: build
build:
	sbcl --load ./scripts/bootstrap.lisp

.PHONY: test
test:
	CHLOROPHYLL_EXIT_ON_FAIL=1 sbcl --load ./scripts/bootstrap-tests.lisp

