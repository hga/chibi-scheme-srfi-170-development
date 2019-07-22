;; Parts of this from the following copyright notices, the total has
;; the same copyright terms, additions and changes Copyright 2019
;; Harold Ancell, Harold Ancell assigns the rights to his additions to
;; Alex Shinn.

;; filesystem.stub -- filesystem bindings
;; Copyright (c) 2009-2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;; 3.1  Errors

(define-record-type syscall-error
  (make-syscall-error errno message procedure data)
  syscall-error?
  (errno syscall-error:errno)
  (message syscall-error:message)
  (procedure syscall-error:procedure)
  (data syscall-error:data))

(define (errno-error errno procedure . data)
    (raise (make-syscall-error errno (integer->error-string errno) procedure data)))

;;; 3.2  I/O

;;; 3.3  File system

(define (delete-directory fname)
  (if (eq? #f (%delete-directory fname))
      (errno-error (errno) delete-directory fname)))

;;> The fundamental directory iterator.  Applies \var{kons} to
;;> each filename in directory \var{dir} and the result of the
;;> previous application, beginning with \var{knil}.  With
;;> \var{kons} as \scheme{cons} and \var{knil} as \scheme{'()},
;;> equivalent to \scheme{directory-files}.

(define (directory-fold dir kons knil)
  (let ((dir (opendir dir)))
    (if (not dir)
        knil
        (let lp ((res knil))
          (let ((file (readdir dir)))
            (if file
                (lp (kons (dirent-name file) res))
                (begin (closedir dir) res)))))))

;;> Returns a list of the files in \var{dir} in an unspecified
;;> order.

(define (directory-files dir)
  (directory-fold dir cons '()))


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
