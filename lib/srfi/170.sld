
(define-library (srfi 170)
  (export

;;; TMP

   ;; 3.5  Process state
   umask set-umask

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
