(define-library (srfi 170 test)
  (export run-tests)

  (import (scheme base)
	  (chibi)
          (chibi test)
          (srfi 170))

  (begin

    (define tmp-containing-dir "/tmp/chibi-scheme-srfi-170-test-xyzzy")
    (define tmp-file1 "/tmp/chibi-scheme-srfi-170-test-xyzzy/file-1")
    (define tmp-file2 "/tmp/chibi-scheme-srfi-170-test-xyzzy/file-2")
    (define tmp-hard-link "/tmp/chibi-scheme-srfi-170-test-xyzzy/hard-link")
    (define tmp-symlink "/tmp/chibi-scheme-srfi-170-test-xyzzy/sym-link")
    (define tmp-fifo "/tmp/chibi-scheme-srfi-170-test-xyzzy/fifo")
    (define tmp-dir "/tmp/chibi-scheme-srfi-170-test-xyzzy/dir")

    (define (run-tests)
      (test-group "srfi-170: POSIX API"

        ;; From 3.5 Process state, to set up for following file system
        ;; tests.

        (test-group "Early, umask, delete old temporary files"

          (test-assert "set-umask" (set-umask #o2))
	  (test "mask" #o2 (umask))

	  (delete-filesystem-object tmp-file1)
	  (delete-filesystem-object tmp-file2)
	  (delete-filesystem-object tmp-hard-link)
	  (delete-filesystem-object tmp-symlink)
	  (delete-filesystem-object tmp-dir)
	  (delete-filesystem-object tmp-fifo)
	  (delete-filesystem-object tmp-containing-dir)

	  ) ; end early

	(test-group "3.1  Errors"

         (test-assert errno/2big) ;; make sure the first of the set exists
         (test-error "errno-error" (errno-error 1 umask))
	 ;; ~~~~ maybe test record predicate and getters???
         ) ; end errors

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

	(test-group "3.5  Process state"

	  ;; umask and set-umask exercised at the very beginning to
	  ;; set up for following file system tests.

          (test-assert "pid" (pid))
	  (test-assert "parent-pid" (parent-pid))

	  (test-assert "working-directory" (string? (working-directory)))

	  ;; ????? ~~~~ set-working-directory exercised at the very beginning to
	  ;; set up for following file system tests.

	  ) ; end process state

	;; 3.6  User and group database access

	;; 3.7  [Intentionally omitted]

	;; 3.8  System parameters

	;; 3.9  Signal system

	;; 3.10  Time

	;; 3.11  Environment variables

	;; 3.12  Terminal device control

	))))
