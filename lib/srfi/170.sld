
(define-library (srfi 170)
  (export posix-time
          make-timespec timespec-seconds timespec-nanoseconds
          timespec?)
  (cond-expand
   (chibi
    (import (chibi))
    (include-shared "170/time"))
   )
)
