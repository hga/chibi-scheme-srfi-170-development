(define-library (srfi 170 test)
  (export run-tests)

  (import (scheme base)
          (chibi)
          (only (chibi process) exit)
          (chibi test)
          (only (chibi filesystem) file-exists?) ;; in R7RS-small
          (only (srfi 1) list-index) ;; list-copy for testing timespecs??
          (srfi 151) ;; bitwise operators
          (srfi 170))

  (include "common.scm")

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
    (define tmp-dot-file "/tmp/chibi-scheme-srfi-170-test-xyzzy/.dot-file")
    (define tmp-dot-file-basename ".dot-file")
    (define tmp-hard-link "/tmp/chibi-scheme-srfi-170-test-xyzzy/hard-link")
    (define tmp-symlink "/tmp/chibi-scheme-srfi-170-test-xyzzy/sym-link")
    (define tmp-no-filesystem-object "/tmp/chibi-scheme-srfi-170-test-xyzzy/no-filesystem-object")

    (define over-max-path "/tmp/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

    (define (delete-tmp-test-files)
      (test-not-error (delete-filesystem-object tmp-dir-1))
      (test-not-error (delete-filesystem-object tmp-dir-2))
      (test-not-error (delete-filesystem-object tmp-fifo))
      (test-not-error (delete-filesystem-object tmp-file-1))
      (test-not-error (delete-filesystem-object tmp-file-2))
      (test-not-error (delete-filesystem-object tmp-dot-file))
      (test-not-error (delete-filesystem-object tmp-hard-link))
      (test-not-error (delete-filesystem-object tmp-symlink))
      (test-not-error (delete-filesystem-object tmp-containing-dir)))

    (define (create-tmp-test-file fname)
      (call-with-output-file fname
        (lambda (out) (display "xyzzy" out)))
      (test-assert (file-exists? fname)))

    (define (is-string-in-list? str lst)
      (list-index (lambda (f) (equal? str f)) lst))


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

          (test-not-error (create-tmp-test-file tmp-dot-file))

;      (define (directory-files dir)

          (test-error (open-directory tmp-no-filesystem-object))

          (let ((di (open-directory tmp-containing-dir)))
            ;; ~~~~ but it will never raise an error.... see 170.stub
            (test-not-error (close-directory di)))

#|
          ;; test open-/read-/close-directory
          (let ((dl (directory-fold tmp-containing-dir cons '())))
            (test-assert (is-string-in-list? "." dl))
            (test-assert (is-string-in-list? ".." dl))
            (test-assert (is-string-in-list? tmp-file-1-basename dl))
            (test-assert (is-string-in-list? tmp-dot-file-basename dl)))
|#


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
            (if (equal? 0 (user-uid))
                (test-not-error (working-directory))
                (test-error (working-directory)))
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

          (test-error (priority priority/user 1))

          ;; assume we're starting out with niceness of 0
          (test 0 (priority priority/process (pid)))
          (test-not-error (set-priority priority/process (pid) 0))

          (if (equal? 0 (user-uid))
              (begin
                (test-not-error (set-priority priority/process (pid) -2))
                (test-not-error (nice (pid) -1)))
              (begin
                (test-error (set-priority priority/process (pid) -2))
                (test-error (nice (pid) -1))))

          (test-not-error (nice (pid) 0))

          ;; setting niceness positive in epilogue to not slow down rest of tests

          (test-assert (string? (user-login-name)))
          (test-assert (> (user-uid) -1))
          (test-assert (> (user-gid) -1))
          (test-assert (list? (user-supplementary-gids))) ;; not sure how to make it fail

          ;; ~~~~~~~~ seriously need better testing for all these set functions
          (if (equal? 0 (user-uid))
              (begin
                (test-not-error (set-uid 0))
                (test-not-error (set-gid 0)))
              (begin
                (test-error (set-uid 0))
                (test-error (set-gid 0))))

          (test-not-error (set-uid (user-uid)))
          (test-not-error (set-gid (user-gid)))

          (test-assert (> (user-effective-uid) -1))
          (test-assert (> (user-effective-gid) -1))
          (if (equal? 0 (user-uid))
              (begin
                (test-not-error (set-user-effective-uid 0))
                (test-not-error (set-user-effective-gid 0)))
              (begin
                (test-error (set-user-effective-uid 0))
                (test-error (set-user-effective-gid 0))))
          (test-not-error (set-user-effective-uid (user-uid)))
          (test-not-error (set-user-effective-gid (user-gid)))
          (test-not-error (set-user-effective-uid (user-effective-uid)))
          (test-not-error (set-user-effective-gid (user-effective-gid)))
          (test-not-error (set-user-real-and-effective-uid -1 -1))
          (test-not-error (set-user-real-and-effective-uid (user-uid) -1))
          (test-not-error (set-user-real-and-effective-uid -1 (user-uid)))
          (test-not-error (set-user-real-and-effective-uid (user-uid) (user-uid)))
          (test-not-error (set-user-real-and-effective-gid -1 -1))
          (test-not-error (set-user-real-and-effective-gid (user-gid) -1))
          (test-not-error (set-user-real-and-effective-gid -1 (user-gid)))
          (test-not-error (set-user-real-and-effective-gid (user-gid) (user-gid)))
          ) ; end process state

        ;; 3.6  User and group database access

;;        (case-expand
;;         ( linux??


        ;; 3.7  [Intentionally omitted]

        (test-group "3.8  System parameters"

          (test-assert (string? (system-name)))

          (test-not-error (uname))
          (let ((un (uname)))
            (test-assert (string? (uname:os-name un)))
            (test-assert (string? (uname:node-name un)))
            (test-assert (string? (uname:release-name un)))
            (test-assert (string? (uname:version un)))
            (test-assert (string? (uname:machine un))))



          )


        (test-group "3.9  Signal system"

          ;; ~~~~ add more tests once we can spawn processes
          (test-not-error (signal-process 0 0))
          (test-not-error (signal-process -1 0))
          (if (not (equal? 0 (user-uid)))
              (test-error (signal-process 1 0)))

          (test-not-error (signal-process-group (process-group) 0))
          ;; very unlikely process 2 both exists and normal user can touch it
          (if (not (equal? 0 (user-uid)))
              (test-error (signal-process-group 2 0)))
          )


        (test-group "3.10  Time"

          (test-not-error (posix-time))
          (test-not-error (monotonic-time))
          (let ((t1 (posix-time))
                (t2 (monotonic-time)))
            (test-assert (and (> (car t1)  0)
                              (> (cdr t1)  0)
                              (> (car t2)  0)
                              (> (cdr t2)  0))))

          (test '(2 . 3) (timespec-difference '(3 . 4) '(1 . 1)))

          ;; timespec=? if we keep it
          )


        (test-group "3.11  Environment variables"

          (test-assert (list? exec-path-list))
          (test-assert (string? (car exec-path-list))))


        ;; 3.12  Terminal device control

        (test-group "Epilogue: set-priority to 2, 3, 4"
          ;; in epilogue so most testing is not slowed down
          (test-assert (set-priority priority/process (pid) 1))
          (test 1 (priority priority/process (pid)))

          (test-not-error (nice))
          (test 2 (priority priority/process (pid)))
          (test-not-error (nice (pid)))
          (test 3 (priority priority/process (pid)))
          (test-not-error (nice (pid) 1))
          (test 4 (priority priority/process (pid)))

          ) ; end epilogue

        ))))
