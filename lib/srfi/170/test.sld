(define-library (srfi 170 test)
  (export run-tests)

  (import (scheme base)
          (chibi)
          (only (chibi process) exit)
          (chibi optional) ;; Snow package for optional args
          (chibi test)
          (only (chibi filesystem)
                  file-exists?  delete-file
                  open open/read open/write open/create open/truncate)
          (only (srfi 1) list-index) ;; list-copy for testing timespecs??
          (only (srfi 115) regexp-split)
          ;; (only (srfi 128) ) ;; comparators (reduced)
          (only (srfi 132) list-sort) ;; sort libraries
          (srfi 151) ;; bitwise operators
          (srfi 170))

  (include "common.scm")
  (include "aux.so")

  (begin

    ;; Inverse of test-error, mutated from test-not, only errors if an
    ;; exception is raised

    (define-syntax test-not-error
      (syntax-rules ()
        ((_ expr) (test-assert (begin expr #t)))
        ((_ name expr) (test-assert name (begin expr #t)))))

    (define the-string-port (open-input-string "plover"))

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
    (define tmp-symlink "/tmp/chibi-scheme-srfi-170-test-xyzzy/symlink")

    (define tmp-no-filesystem-object "/tmp/chibi-scheme-srfi-170-test-xyzzy/no-filesystem-object")
    (define bogus-path "/foo/bar/baz/quux")

    (define the-text-string "The quick brown fox jumps over the lazy quux")
    (define the-text-string-length (string-length the-text-string))
    (define the-binary-bytevector (bytevector 0 1 2 3 4 5 6 7 8 9))
    (define the-binary-bytevector-length (bytevector-length the-binary-bytevector))
    (define open-write-create-truncate (bitwise-ior open/write open/create open/truncate))

    (define starting-dir (working-directory))

    (define no-dot (list-sort string<? '("fifo" "file-1" "hard-link" "symlink")))
    (define with-dot (list-sort string<? '(".dot-file" "fifo" "file-1" "hard-link" "symlink")))

    (define over-max-path "/tmp/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

    (define (delete-tmp-test-files)
      (test-not-error (delete-filesystem-object tmp-dir-1))
      (test-not-error (delete-filesystem-object tmp-dir-2))
      (test-not-error (delete-filesystem-object tmp-fifo))
      (test-not-error (delete-filesystem-object tmp-symlink)) ;; up here to avoid problem with file-exists? check
      (test-not-error (delete-filesystem-object tmp-file-1))
      (test-not-error (delete-filesystem-object tmp-file-2))
      (test-not-error (delete-filesystem-object tmp-dot-file))
      (test-not-error (delete-filesystem-object tmp-hard-link))
      (test-not-error (delete-filesystem-object tmp-containing-dir)))

    (define (create-tmp-test-file fname)
      (call-with-output-file fname
        (lambda (out) (display "plugh" out)))
      (test-assert (file-exists? fname)))

    (define (is-string-in-list? str lst)
      (list-index (lambda (f) (equal? str f)) lst))


    (define (run-tests)
      (test-group "srfi-170: POSIX API"

        ;; From 3.5 Process state, to set up for following file system
        ;; tests.

        (test-group "Prologue: umask, delete-filesystem-object any old temporary files and directories"

          ;; ~~~~~~~~ need to test that PATH_MAX is no larger than 4096
          ;; ~~~~~~~~ need to test that term/l-ctermid is no larger than 1024


          (test 0 (errno))
          (test-not-error (set-errno errno/2big))
          (set-errno errno/2big)
          (test errno/2big (errno))
          (test-assert (string? (integer->error-string)))
          (test-assert (string? (integer->error-string errno/2big)))
          (set-errno errno/2big)
          (test-assert (equal? (integer->error-string) (integer->error-string errno/2big)))

          (delete-tmp-test-files)

          (test-assert (set-umask #o2))
          (test #o2 (umask))

          ;; create containing directory so we'll have a place for 3.2  I/O
          (test-not-error (create-directory tmp-containing-dir))
          (test #o775 (bitwise-and (file-info:mode (file-info tmp-containing-dir #t)) #o777)) ; test umask
          (test-assert (file-exists? tmp-containing-dir))
          (test-not-error (create-directory tmp-containing-dir #o755 #t))
          (test-assert (file-exists? tmp-containing-dir))
          (test #o755 (bitwise-and (file-info:mode (file-info tmp-containing-dir #t)) #o777))
          ) ; end early

        (test-group "3.1  Errors"

         (test-assert errno/2big) ;; make sure the first of the set exists
         (test-error (errno-error 1 umask))

         ;; ~~~~ maybe test record predicate and getters???
         ) ; end errors


        (test-group "3.2  I/O"

          (let ((the-port (fdes->binary-output-port
                           (%fileno-to-fd (open tmp-file-1 open-write-create-truncate)))))
            (test-not-error (write-bytevector the-binary-bytevector the-port))
            (test-not-error (close-port the-port)))
          (let ((the-port (fdes->binary-input-port
                           (%fileno-to-fd (open tmp-file-1 open/read)))))
            (test-assert (equal? the-binary-bytevector (read-bytevector the-binary-bytevector-length the-port)))
            (test-assert (eof-object? (read-char the-port)))
            (test-not-error (close-port the-port)))

          (let ((the-port (fdes->textual-output-port
                           (%fileno-to-fd (open tmp-file-1 open-write-create-truncate)))))
            (test-not-error (write-string the-text-string the-port))
            (test-not-error (close-port the-port)))
          (let ((the-port (fdes->textual-input-port
                           (%fileno-to-fd (open tmp-file-1 open/read)))))
            (test-assert (equal? the-text-string (read-string the-text-string-length the-port)))
            (test-assert (eof-object? (read-char the-port)))
            (test-not-error (close-port the-port)))

          (test 0 (port-fdes (current-input-port)))
          (test 1 (port-fdes (current-output-port)))
          (test 2 (port-fdes (current-error-port)))
          (test-not (port-fdes the-string-port))

          (let* ((dev-zero-fileno (open "/dev/zero" open/read)) ;; fileno type object
                 (dev-zero-fd (%fileno-to-fd dev-zero-fileno)))
            (test-not-error (close-fdes dev-zero-fd))
            (test-error (close-fdes dev-zero-fd)))
          )


        (test-group "3.3  File system"

          ;; ~~~~ test (not override) for the following
          ;; ~~~~ test when from-fname does not exist
          ;; ~~~~ test across filesystems, assuming /var is not in same as /tmp
          ;; ~~~~ do some time sanity checking, e.g. get time, subtract a few seconds, test....

          (test-not-error (create-directory tmp-dir-1))
          (test-assert (file-exists? tmp-dir-1))

          (test-not-error (create-fifo tmp-fifo))
          (test-assert (file-exists? tmp-fifo))
          (test-not-error (create-fifo tmp-fifo #o644 #t))
          (test-assert (file-exists? tmp-fifo))
          (test #o644 (bitwise-and (file-info:mode (file-info tmp-fifo #t)) #o777))

          ;; (test-not-error (create-tmp-test-file tmp-file-1)) ;; created above in I/O

          (test-not-error (create-hard-link tmp-file-1 tmp-hard-link))
          (test-assert (file-exists? tmp-hard-link))
          (test-not-error (create-hard-link tmp-file-1 tmp-hard-link #t))
          (test-assert (file-exists? tmp-hard-link))

          (test-not-error (create-symlink tmp-file-1 tmp-symlink))
          (test-assert (file-exists? tmp-symlink))
          (test-not-error (create-symlink tmp-file-1 tmp-symlink #t))
          (test-assert (file-exists? tmp-symlink))
          (test-assert (equal? (file-info:inode (file-info tmp-file-1 #t))
                               (file-info:inode (file-info tmp-symlink #t))))
          (test #f (equal? (file-info:inode (file-info tmp-file-1 #t))
                           (file-info:inode (file-info tmp-symlink #f))))

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

          (test-error (read-symlink tmp-file-1))
          (test tmp-file-1 (read-symlink tmp-symlink))

          (test-not-error (delete-directory tmp-dir-1))

          (test-not-error (set-file-mode tmp-file-1 #o744))
          (test #o744 (bitwise-and (file-info:mode (file-info tmp-file-1 #t)) #o777))

          (let* ((fi-starting (file-info tmp-file-1 #t))
                 (my-starting-uid (file-info:uid fi-starting))
                 (my-starting-gid (file-info:gid fi-starting)))
#|
            (test-not-error (set-file-owner tmp-file-1 my-starting-uid)) ; best we can do, not assuming we're root!
            (test-not-error (set-file-group tmp-file-1 my-starting-gid)) ; maybe see what supplementary groups we have?
|#
            (let ((fi-ending (file-info tmp-file-1 #t)))
              (test my-starting-uid (file-info:uid fi-ending))
              (test my-starting-gid (file-info:gid fi-ending))))

          (test the-text-string-length (file-info:size (file-info tmp-file-1  #t)))
          (test-not-error (truncate-file tmp-file-1 30))
          (test 30 (file-info:size (file-info tmp-file-1 #t)))
          (let ((the-port (open-output-file tmp-file-1))) ;; note this truncates the file to 0 length
            (test-not-error (truncate-file the-port 10)) ;; this should make the file 10 bytes of 0s
            (test 10 (file-info:size (file-info tmp-file-1 #t)))
            (test-not-error (close-output-port the-port)))

          ;; test remaining file-info features
          (let ((fi (file-info tmp-file-1 #t)))
            (test-assert (file-info? fi))
            (test 2 (file-info:nlinks fi))
            ;; ~~~~ test uid and gid
            (test-assert (pair? (file-info:atime fi)))
            (test-assert (pair? (file-info:mtime fi)))
            (test-assert (pair? (file-info:ctime fi)))
            (test-not (file-info-directory? fi))
            (test-not (file-info-fifo? fi))
            (test-not (file-info-symlink? fi))
            (test-assert (file-info-regular? fi))
            )

          (let* ((the-port (open-input-file tmp-file-1))
                 (fi (file-info the-port 'follow-is-ignored)))
            (test-assert (file-info? fi))
            (test 2 (file-info:nlinks fi))
            ;; ~~~~ test uid and gid
            (test-assert (pair? (file-info:atime fi)))
            (test-assert (pair? (file-info:mtime fi)))
            (test-assert (pair? (file-info:ctime fi)))
            (test-not (file-info-directory? fi))
            (test-not (file-info-fifo? fi))
            (test-not (file-info-symlink? fi))
            (test-assert (file-info-regular? fi))
            (test-not-error (close-input-port the-port))
            )

          (test-assert (file-info-directory? (file-info tmp-containing-dir #t)))
          (test-assert (file-info-fifo? (file-info tmp-fifo #t)))
          (test-assert (file-info-symlink? (file-info tmp-symlink #f)))

          (test-not-error (create-tmp-test-file tmp-dot-file))

          ;; test normal function of open-/read-/close-directory as well
          (test-assert (equal? no-dot (list-sort string<? (directory-files tmp-containing-dir))))
          (test-assert (equal? with-dot (list-sort string<? (directory-files tmp-containing-dir #t))))

          (test-not-error (set-working-directory tmp-containing-dir))
          (test-assert (equal? no-dot (list-sort string<? (directory-files))))
          (test-not-error (set-working-directory starting-dir))

          (test-error (open-directory tmp-no-filesystem-object))
          (let ((dirobj (open-directory tmp-containing-dir)))
            (test-not-error (close-directory dirobj)))

          ;; ~~~~ add full set of error cases

#|
          ;; test open-/read-/close-directory
          (let ((dl (directory-fold tmp-containing-dir cons '())))
            (test-assert (is-string-in-list? "." dl))
            (test-assert (is-string-in-list? ".." dl))
            (test-assert (is-string-in-list? tmp-file-1-basename dl))
            (test-assert (is-string-in-list? tmp-dot-file-basename dl)))
|#

          (test-not-error (set-working-directory tmp-containing-dir))
          (test tmp-containing-dir (real-path "."))
          (test tmp-file-1 (real-path tmp-file-1-basename))
          (test tmp-file-1 (real-path (string-append "./" tmp-file-1-basename)))
          ;; ~~~~ we'll trust it resolves symlinks, can't actually do anything if it doesn't....
          (test-error (real-path bogus-path))
          (test-not-error (set-working-directory starting-dir))

          (let ((tmp-filename (temp-file-prefix)))
            (test-assert (string? tmp-filename))
            (temp-file-prefix #t) ;; parementer object, and argument ignored
            (test-assert (not (equal? tmp-filename (temp-file-prefix)))))

          ;; ~~~~ this doesn't test skipping past an existing temp file....
          (let ((the-filename (create-temp-file)))
            (test-assert (file-exists? the-filename))
            (test-not-error (delete-file the-filename))) ;; clean up after self, but bad for debugging
          (if (not (equal? 0 (user-uid)))
              (test-error (create-temp-file "/xyzzy-plover-plugh.")))



          )


        (test-group "3.5  Process state"

          ;; umask and set-umask exercised at the very beginning to
          ;; set up for following file system tests.

          (test-assert (string? (working-directory)))
          (test-error (set-working-directory over-max-path))
          (test-not-error (set-working-directory tmp-containing-dir))
          (test tmp-containing-dir (working-directory))
          (test-not-error (file-info tmp-file-1-basename #t)) ; are we there?

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

          (test 0 (nice 0))

          ;; setting niceness positive in epilogue to not slow down rest of tests

          (test-assert (> (user-uid) -1))
          (test-assert (> (user-gid) -1))
          (test-assert (> (user-effective-uid) -1))
          (test-assert (> (user-effective-gid) -1))
          (test-assert (list? (user-supplementary-gids))) ;; not sure how to make it fail
          ) ; end process state


        (test-group "3.6  User and group database access"

          (test-assert (user-info? (user-info 0)))
          (test 0 (user-info:uid (user-info 0)))
          (test-assert (user-info? (user-info "root")))
          (test 0 (user-info:uid (user-info "root")))

          (let ((the-parsed-user-name (user-info:parsed-full-name (user-info 0))))
            (test-assert (list? the-parsed-user-name))
            (test-assert (string? (car the-parsed-user-name))))

          (test '("Test User" "" "" "") (parse-gecos "Test User,,," "test"))
          ;; ~~~~ test ampersand substitution in parse-gecos

          (test-assert (group-info? (group-info 0)))
          (test 0 (group-info:gid (group-info 0)))
          ;; group 0 is wheel on OpenBSD, daemon works for it and Bionic Beaver
          (test-assert (group-info? (group-info "daemon")))
          (test 1 (group-info:gid (group-info "daemon")))
          ) ; end user and group database access


        (test-group "3.10  Time"

          (test-not-error (posix-time))
          (test-not-error (monotonic-time))
          (let ((t1 (posix-time))
                (t2 (monotonic-time)))
            (test-assert (and (> (car t1)  0)
                              (> (cdr t1)  0)
                              (> (car t2)  0)
                              (> (cdr t2)  0))))
          )


        (test-group "3.12  Terminal device control"

          (test-assert (terminal? (current-input-port)))
          (let ((port-not-terminal (open-input-file tmp-file-1)))
            (test-not (terminal? port-not-terminal))
            (close-port port-not-terminal))

          (test-error (terminal-file-name 1))
          (test-error (terminal-file-name the-string-port))
          (let ((port-not-terminal (open-input-file tmp-file-1)))
            (test-error (terminal-file-name port-not-terminal))
            (close-port port-not-terminal))
          (test-assert (string? (terminal-file-name (current-input-port))))
          (test-assert (string? (terminal-file-name (current-output-port))))
          (test-assert (string? (terminal-file-name (current-error-port))))

          ;; These with- and without- tests only test errors and
          ;; getting to and out of the supplied proc, not the actual
          ;; detailed terminal mode

          (test-error (with-raw-mode 1 (current-output-port) 2 4 (lambda (x y) 'something-for-body)))
          (test-error (with-raw-mode (current-input-port) 1 2 4 (lambda (x y) 'something-for-body)))
          (test-error (with-raw-mode the-string-port (current-output-port) 2 4 (lambda (x y) 'something-for-body)))
          (test-error (with-raw-mode (current-input-port) the-string-port 2 4 (lambda (x y) 'something-for-body)))
          (test-error (with-raw-mode (current-output-port) (current-input-port) 2 4 (lambda (x y) 'something-for-body)))
          ;; ~~~~ test for a file descriptor in port???
          (test 'something-for-body (with-raw-mode (current-input-port) (current-output-port) 2 4 (lambda (x y) 'something-for-body)))

          (test-error (with-rare-mode 1 (current-output-port) (lambda (x y) 'something-for-body)))
          (test-error (with-rare-mode (current-input-port) 1 (lambda (x y) 'something-for-body)))
          (test-error (with-rare-mode the-string-port (current-output-port) (lambda (x y) 'something-for-body)))
          (test-error (with-rare-mode (current-input-port) the-string-port (lambda (x y) 'something-for-body)))
          (test-error (with-rare-mode (current-output-port) (current-input-port) (lambda (x y) 'something-for-body)))
          ;; ~~~~ test for a file descriptor in port???
          (test 'something-for-body (with-rare-mode (current-input-port) (current-output-port) (lambda (x y) 'something-for-body)))

          (test-error (without-echo 1 (current-output-port) (lambda (x y) 'something-for-body)))
          (test-error (without-echo (current-input-port) 1 (lambda (x y) 'something-for-body)))
          (test-error (without-echo the-string-port (current-output-port) (lambda (x y) 'something-for-body)))
          (test-error (without-echo (current-input-port) the-string-port (lambda (x y) 'something-for-body)))
          (test-error (without-echo (current-output-port) (current-input-port) (lambda (x y) 'something-for-body)))
          ;; ~~~~ test for a file descriptor in port???
          (test 'something-for-body (without-echo (current-input-port) (current-output-port) (lambda (x y) 'something-for-body)))

          )

        (test-group "Epilogue: set-priority to 1, 2, 4"

          (close-port the-string-port)

          ;; in epilogue so most testing is not slowed down

          (test 1 (nice))
          (test 2 (nice 1))
          (test 4 (nice 2))

          ) ; end epilogue

        ))))
