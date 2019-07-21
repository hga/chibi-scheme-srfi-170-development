
(define-library (srfi 170)
  (export

;;; TMP

   ;; 3.1  Errors

   syscall-error?
   syscall-error:errno syscall-error:message
   syscall-error:procedure syscall-error:data
   errno-error

   ;; 3.2  I/O

   ;; 3.3  File system

   ;; 3.4  Processes

   ;; 3.4.1  Process objects

   ;; 3.4.2  Process waiting

   ;; 3.4.3  Analysing process status codes

   ;; 3.5  Process state
   umask set-umask

   ;; 3.6  User and group database access

   ;; 3.7  [Intentionally omitted]

   ;; 3.8  System parameters

   ;; 3.9  Signal system

   ;; 3.10  Time
   clock-gettime
   clck-id/realtime clck-id/monotonic
   make-timespec timespec-seconds timespec-nanoseconds
   timespec?

   ;; 3.11  Environment variables

   ;; 3.12  Terminal device control

   )
  (cond-expand
   (chibi
    (import (scheme base))
    (import (chibi))
    (import (only (chibi ast) errno integer->error-string)) ;; ~~~~  until aux.c is up to snuff
    (include-shared "170/170"))) ;; ~~~~ add aux when it's up to snuff
  (include "170/170.scm"))
