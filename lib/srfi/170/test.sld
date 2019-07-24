(define-library (srfi 170 test)
  (export run-tests)

  (import (chibi)
  	  (chibi test)
	  (srfi 170))

  (begin
    (define (run-tests)
      (test-begin "srfi-170: POSIX API")

      (test-error (file-info "dsfhsdfhi39287935lscoikj864873648364"))

      (test-end))))
