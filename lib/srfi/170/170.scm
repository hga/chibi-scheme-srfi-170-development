;;; 3.1  Errors

  ;; ~~~~ Impliment!


;;; 3.2  I/O

;;; 3.3  File system



;;; 3.4  Processes

;;; 3.4.1  Process objects

;;; 3.4.2  Process waiting

;;; 3.4.3  Analysing process status codes

;;; 3.5  Process state

(define (umask)
  (let ((current-umask (%umask #o777)))
    (%umask current-umask)
    current-umask))

(define (set-umask perms)
  (%umask perms))

;;; 3.6  User and group database access

;;; 3.7  [Intentionally omitted]

;;; 3.8  System parameters

;;; 3.9  Signal system

;;; 3.10  Time

  ;; Be sure to raise exception if wrong clock!



;;; 3.11  Environment variables

;;; 3.12  Terminal device control
