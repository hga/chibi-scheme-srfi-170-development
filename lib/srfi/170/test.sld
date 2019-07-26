(define-library (srfi 170 test)
  (export run-tests)

  (import (scheme base)
          (chibi)
          (only (chibi process) exit)
          (chibi test)
          (only (chibi filesystem) file-exists?) ;; in R7RS-small
          (srfi 151) ;; bitwise operators
          (srfi 170))

  (begin

    ;; Inverse of test-error, mutated from test-not, only errors if an
    ;; exception is raised

    (define-syntax test-not-error
      (syntax-rules ()
        ((_ expr) (test-assert (begin expr #t)))
        ((_ name expr) (test-assert name (begin expr #t)))))

    (define tmp-containing-dir "/tmp/chibi-scheme-srfi-170-test-xyzzy")
    (define tmp-dir "/tmp/chibi-scheme-srfi-170-test-xyzzy/dir")
    (define tmp-fifo "/tmp/chibi-scheme-srfi-170-test-xyzzy/fifo")
    (define tmp-file1 "/tmp/chibi-scheme-srfi-170-test-xyzzy/file-1")
;;    (define tmp-file2 "/tmp/chibi-scheme-srfi-170-test-xyzzy/file-2")
    (define tmp-hard-link "/tmp/chibi-scheme-srfi-170-test-xyzzy/hard-link")
    (define tmp-symlink "/tmp/chibi-scheme-srfi-170-test-xyzzy/sym-link")

    (define (delete-tmp-test-files)
      (test-not-error (delete-filesystem-object tmp-dir))
      (test-not-error (delete-filesystem-object tmp-fifo))
      (test-not-error (delete-filesystem-object tmp-file1))
;;      (test-not-error (delete-filesystem-object tmp-file2))
      (test-not-error (delete-filesystem-object tmp-hard-link))
      (test-not-error (delete-filesystem-object tmp-symlink))
      (test-not-error (delete-filesystem-object tmp-containing-dir)))

    (define (create-tmp-test-file fname)
      (call-with-output-file fname
        (lambda (out) (display "xyzzy" out)))
      (test-assert (file-exists? fname)))


    (define (run-tests)
      (test-group "srfi-170: POSIX API"

        ;; From 3.5 Process state, to set up for following file system
        ;; tests.

        (test-group "Prologue: umask, delete-filesystem-object any old temporary files and directories"

          (delete-tmp-test-files)

          (test-assert (set-umask #o2))
          (test #o2 (umask))
          ) ; end early

        (test-group "3.1  Errors"

         (test-assert errno/2big) ;; make sure the first of the set exists
         (test-error (errno-error 1 umask))

         ;; ~~~~ maybe test record predicate and getters???
         ) ; end errors

        ;; 3.2  I/O

        (test-group "3.3  File system"

          ;; ~~~~ test permission bits and override for the following

          (test-not-error (create-directory tmp-containing-dir))
          (test #o775 (bitwise-and (file-info:mode (file-info tmp-containing-dir)) #o777)) ; test umask
          (test-assert (file-exists? tmp-containing-dir))
          (test-not-error (create-directory tmp-containing-dir #o755 #t))
          (test-assert (file-exists? tmp-containing-dir))
          (test #o755 (bitwise-and (file-info:mode (file-info tmp-containing-dir)) #o777))

          (test-not-error (create-directory tmp-dir))
          (test-assert (file-exists? tmp-dir))

          (test-not-error (create-fifo tmp-fifo))
          (test-assert (file-exists? tmp-fifo))
          (test-not-error (create-fifo tmp-fifo #o644 #t))
          (test-assert (file-exists? tmp-fifo))
          (test #o644 (bitwise-and (file-info:mode (file-info tmp-fifo)) #o777))

          (create-tmp-test-file tmp-file1)

          (test-not-error (create-hard-link tmp-file1 tmp-hard-link))
          (test-assert (file-exists? tmp-hard-link))
          (test-not-error (create-hard-link tmp-file1 tmp-hard-link #t))
          (test-assert (file-exists? tmp-hard-link))

          (test-not-error (create-symlink tmp-file1 tmp-symlink))
          (test-assert (file-exists? tmp-symlink))
          (test-not-error (create-symlink tmp-file1 tmp-symlink #t))
          (test-assert (file-exists? tmp-symlink))

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

          )

        ;; 3.4  Processes

        ;; 3.4.1  Process objects

        ;; 3.4.2  Process waiting

        ;; 3.4.3  Analysing process status codes

        (test-group "3.5  Process state"

          ;; umask and set-umask exercised at the very beginning to
          ;; set up for following file system tests.

          (test-assert (pid))
          (test-assert (parent-pid))

          (test-assert (string? (working-directory)))

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

;; Leave the files around for debugging test errors
;;        (test-group "Epilogue: delete-filesystem-object any temporary files and directories left"
;;          (delete-tmp-test-files))

        ))))
