
(define-library (srfi 170)
  (export

;; 3.10  Time
   clock-gettime
   clck-id/realtime clck-id/monotonic
   make-timespec timespec-seconds timespec-nanoseconds
   timespec?

   )
  (cond-expand
   (chibi
    (import (chibi))
    (include-shared "170/170")))
  (include "170/170.scm"))
