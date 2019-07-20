
(define-library (srfi 170)
  (export

;;; TMP

   ;; 3.1  Errors

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
    (import (chibi))
    (include-shared "170/170")))
  (include "170/170.scm"))
