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
    (define tmp-dir-1 "/tmp/chibi-scheme-srfi-170-test-xyzzy/dir-1")
    (define tmp-dir-2 "/tmp/chibi-scheme-srfi-170-test-xyzzy/dir-2")
    (define tmp-fifo "/tmp/chibi-scheme-srfi-170-test-xyzzy/fifo")
    (define tmp-file-1 "/tmp/chibi-scheme-srfi-170-test-xyzzy/file-1")
    (define tmp-file-1-basename "file-1")
    (define tmp-file-2 "/tmp/chibi-scheme-srfi-170-test-xyzzy/file-2")
    (define tmp-hard-link "/tmp/chibi-scheme-srfi-170-test-xyzzy/hard-link")
    (define tmp-symlink "/tmp/chibi-scheme-srfi-170-test-xyzzy/sym-link")

    (define over-max-path "/tmp/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

    (define (delete-tmp-test-files)
      (test-not-error (delete-filesystem-object tmp-dir-1))
      (test-not-error (delete-filesystem-object tmp-dir-2))
      (test-not-error (delete-filesystem-object tmp-fifo))
      (test-not-error (delete-filesystem-object tmp-file-1))
      (test-not-error (delete-filesystem-object tmp-file-2))
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

          ;; ~~~~ test (not override) for the following
          ;; ~~~~ test when from-fname does not exist
          ;; ~~~~ test across filesystems, assuming /var is not in same as /tmp
          ;; ~~~~ do some time sanity checking, e.g. get time, subtract a few seconds, test....

          (test-not-error (create-directory tmp-containing-dir))
          (test #o775 (bitwise-and (file-info:mode (file-info tmp-containing-dir)) #o777)) ; test umask
          (test-assert (file-exists? tmp-containing-dir))
          (test-not-error (create-directory tmp-containing-dir #o755 #t))
          (test-assert (file-exists? tmp-containing-dir))
          (test #o755 (bitwise-and (file-info:mode (file-info tmp-containing-dir)) #o777))

          (test-not-error (create-directory tmp-dir-1))
          (test-assert (file-exists? tmp-dir-1))

          (test-not-error (create-fifo tmp-fifo))
          (test-assert (file-exists? tmp-fifo))
          (test-not-error (create-fifo tmp-fifo #o644 #t))
          (test-assert (file-exists? tmp-fifo))
          (test #o644 (bitwise-and (file-info:mode (file-info tmp-fifo)) #o777))

          (test-not-error (create-tmp-test-file tmp-file-1))

          (test-not-error (create-hard-link tmp-file-1 tmp-hard-link))
          (test-assert (file-exists? tmp-hard-link))
          (test-not-error (create-hard-link tmp-file-1 tmp-hard-link #t))
          (test-assert (file-exists? tmp-hard-link))

          (test-not-error (create-symlink tmp-file-1 tmp-symlink))
          (test-assert (file-exists? tmp-symlink))
          (test-not-error (create-symlink tmp-file-1 tmp-symlink #t))
          (test-assert (file-exists? tmp-symlink))

          (test-not-error (rename-file tmp-file-1 tmp-file-2))
          (test-assert (file-exists? tmp-file-2))
          (test-not (file-exists? tmp-file-1))
          (test-not-error (create-tmp-test-file tmp-file-1))
          (test-not-error (rename-file tmp-file-2 tmp-file-1 #t))
          (test-assert (file-exists? tmp-file-1))
          (test-not (file-exists? tmp-file-2))

          (test-not-error (rename-file tmp-dir-1 tmp-dir-2))
          (test-assert (file-exists? tmp-dir-2))
          (test-not (file-exists? tmp-dir-1))
          (test-not-error (create-directory tmp-dir-1))
          (test-error (rename-file tmp-dir-2 tmp-file-1))
          (test-not-error (rename-file tmp-dir-2 tmp-dir-1 #t))
          (test-assert (file-exists? tmp-dir-1))
          (test-not (file-exists? tmp-dir-2))

          (test-not-error (delete-directory tmp-dir-1))

          (test-not-error (set-file-mode tmp-file-1 #o744))
          (test #o744 (bitwise-and (file-info:mode (file-info tmp-file-1)) #o777))

          (let* ((fi-starting (file-info tmp-file-1))
                 (my-starting-uid (file-info:uid fi-starting))
                 (my-starting-gid (file-info:gid fi-starting)))

            (test-not-error (set-file-owner tmp-file-1 my-starting-uid)) ; best we can do, not assuming we're root!
            (test-not-error (set-file-group tmp-file-1 my-starting-gid)) ; maybe see what supplementary groups we have?

            (let ((fi-ending (file-info tmp-file-1)))
              (test my-starting-uid (file-info:uid fi-ending))
              (test my-starting-gid (file-info:gid fi-ending))))

          ;; test chasing
          (let* ((fi-starting (file-info tmp-file-1 #f))
                 (my-starting-uid (file-info:uid fi-starting))
                 (my-starting-gid (file-info:gid fi-starting)))

            (test-not-error (set-file-owner tmp-file-1 my-starting-uid #f)) ; best we can do, not assuming we're root!
            (test-not-error (set-file-group tmp-file-1 my-starting-gid #f)) ; maybe see what supplementary groups we have?

            (let ((fi-ending (file-info tmp-file-1 #f)))
              (test my-starting-uid (file-info:uid fi-ending))
              (test my-starting-gid (file-info:gid fi-ending))))

          (test 5 (file-info:size (file-info tmp-file-1)))
          (test-not-error (truncate-file tmp-file-1 3))
          (test 3 (file-info:size (file-info tmp-file-1)))

          ;; test remaining file-info features
          (let ((fi (file-info tmp-file-1 #f)))
            (test-assert (file-info? fi))
            (test 2 (file-info:nlinks fi))
            ;; ~~~~ test uid and gid
            (test-assert (pair? (file-info:atime fi)))
            (test-assert (pair? (file-info:mtime fi)))
            (test-assert (pair? (file-info:ctime fi)))
            (test-not (file-info-directory? fi))
            (test-not (file-info-fifo? fi))
            (test-assert (file-info-regular? fi))
;;            (test-not (file-info-socket? fi)) ~~~~ fails on Linux and OpenBSD???
            (test-not (file-info-block-special? fi))
            (test-not (file-info-character-special? fi))
;;            (test-not (file-info-symlink? fi)) ~~~~ fails on Linux and OpenBSD???
            )

          (test-assert (file-info-directory? (file-info tmp-containing-dir)))
          (test-assert (file-info-fifo? (file-info tmp-fifo)))
;;      (define (file-info-block-special? file-info-record) no fixed set of these
;;          (test-not (file-info-block-special? (file-info "/dev/tty"))) ~~~~ fails on Linux and OpenBSD???
          (test-assert (file-info-character-special? (file-info "/dev/tty")))
          (test-assert (file-info-symlink? (file-info tmp-symlink)))

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

          (test-assert (string? (working-directory)))
          (test-error (set-working-directory over-max-path))
          (test-not-error (set-working-directory tmp-containing-dir))
          (test tmp-containing-dir (working-directory))
          (test-not-error (file-info tmp-file-1-basename)) ; are we there?

          (cond-expand (bsd
            (test-not-error (set-file-mode tmp-containing-dir #o000))
            (test-error (working-directory))
            (test-not-error (set-file-mode tmp-containing-dir #o755))))

          (test-assert (pid))
          (test-assert (parent-pid))

          (test-not-error (process-group))
          (cond-expand
           (linux (test 1 (process-group 1))) ;; may not work on non-Ubuntu Linuxes
           (bsd (test-error (process-group 1)))) ;; fails on OpenBSD AMD64 6.5
          (test-error (process-group -1))
          (test-not-error (set-process-group (process-group))) ;; ~~~~ can we do better?
          (test-not-error (set-process-group 0 (process-group))) ;; ~~~~ can we do better?

          ;; this might succeed if you are root
          (test-error (priority priority/user 1))
          ;; assume we're starting out with niceness of 0
          (test 0 (priority priority/process (pid)))

          (test-error (set-priority priority/process (pid) -2))
          ;; setting niceness positive in epilogue to not slow down rest of tests

          ) ; end process state

        ;; 3.6  User and group database access

        ;; 3.7  [Intentionally omitted]

        ;; 3.8  System parameters

        ;; 3.9  Signal system

        ;; 3.10  Time

        ;; 3.11  Environment variables

        ;; 3.12  Terminal device control

        (test-group "Epilogue: set-priority 1"
          ;; in epilogue so most testing is not slowed down
          (test-assert (set-priority priority/process (pid) 1))
          (test 1 (priority priority/process (pid)))

          ) ; end epilogue

        ))))
