(define-library (srfi 170 test)
  (export run-tests)

  (import (scheme base)
;;;       (chibi)
          (chibi test)
          (srfi 170))

  (begin
    ;; ~~~~ insert prefatory definitions here
    (define (run-tests)
      (test-group "srfi-170: POSIX API"

        ;; From 3.5 Process state, to set up for following file system
        ;; tests.  Also the simpelist possible system call that by
        ;; definition can't fail

        (test-group "Early, umask"

           (test-assert "set-umask" (set-umask #o2))
           (test "mask" #o2 (umask))
           ) ; end umask

      ;; 3.1  Errors

;;;      (define (errno-error errno procedure . data)


      ;; 3.2  I/O

      ;; 3.3  File system

      ;; Helper function; unlike scsh version, will raise an exception if
      ;; there is a object it can't delete.  If no object, no exception.

;;;      (define (delete-filesystem-object fname)


;;;      (define (delete-directory fname)


;;;      (define (file-info fname/port . o)

;;      (test-error (file-info "dsfhsdfhi39287935lscoikj864873648364"))

;;;      (define (file-info-directory? file-info-record)

;;;      (define (file-info-fifo? file-info-record)

;;;      (define (file-info-regular? file-info-record)

;;;      (define (file-info-socket? file-info-record)

;;;      (define (file-info-block-special? file-info-record)

;;;      (define (file-info-character-special? file-info-record)

;;;      (define (file-info-symlink? file-info-record)


      ;; ----------------

      ;;> The fundamental directory iterator.  Applies \var{kons} to
      ;;> each filename in directory \var{dir} and the result of the
      ;;> previous application, beginning with \var{knil}.  With
      ;;> \var{kons} as \scheme{cons} and \var{knil} as \scheme{'()},
      ;;> equivalent to \scheme{directory-files}.

;      (define (directory-fold dir kons knil)

      ;;> Returns a list of the files in \var{dir} in an unspecified
      ;;> order.

;      (define (directory-files dir)

      ;; 3.4  Processes

      ;; 3.4.1  Process objects

      ;; 3.4.2  Process waiting

      ;; 3.4.3  Analysing process status codes

      ;; 3.5  Process state

      ;; umask and set-umask exercised at the very beginning to set up
      ;; for following file system tests.  Also the simpelist possible
      ;; system call that by definition can't fail

      ;; 3.6  User and group database access

      ;; 3.7  [Intentionally omitted]

      ;; 3.8  System parameters

      ;; 3.9  Signal system

      ;; 3.10  Time

      ;; 3.11  Environment variables

      ;; 3.12  Terminal device control

	))))
