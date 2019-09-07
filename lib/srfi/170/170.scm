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
    (raise-continuable (make-syscall-error errno (integer->error-string errno) procedure data)))

(define (retry-if-EINTR the-lambda)
  (let loop ((ret (the-lambda)))
    (if ret
        ret
        (if (equal? errno/intr (errno))
            (loop (the-lambda))
            ret))))


;;; 3.2  I/O

;; ~~~~ seems Chibi handles bogus fds OK, reading input returns eof,
;; output raises errors

(define (fdes->textual-input-port the-fd)
  (%file_descriptor_to_port the-fd #t #f))

(define (fdes->binary-input-port the-fd)
  (%file_descriptor_to_port the-fd #t #t))

(define (fdes->textual-output-port the-fd)
  (%file_descriptor_to_port the-fd #f #f))

(define (fdes->binary-output-port the-fd)
  (%file_descriptor_to_port the-fd #f #t))

(define (port-fdes the-port)
  (if (not (port? the-port))
      (errno-error errno/inval port-fdes the-port))
  (port-fileno the-port))

(define (close-fdes the-fd)
  (if (or (not (fixnum? the-fd)) (< the-fd 0))
      (errno-error errno/inval close-fdes the-fd))
  (if (not (retry-if-EINTR (lambda () (%close the-fd))))
      (errno-error (errno) close-fdes the-fd)))


;;; 3.3  File system

(define (create-directory fname . o)
  (let-optionals o ((permission-bits #o775)
                    (override? #f))
    (if override? (delete-filesystem-object fname))
    (if (not (%mkdir fname permission-bits))
        (errno-error (errno) create-directory fname))))

(define (create-fifo fname . o)
  (let-optionals o ((permission-bits #o664)
                    (override? #f))
    (if override? (delete-filesystem-object fname))
    (if (not (%mkfifo fname permission-bits))
        (errno-error (errno) create-fifo fname))))

(define (create-hard-link oldname newname . o)
  (let-optionals o ((override? #f))
    (if override? (delete-filesystem-object newname))
    (if (not (%link oldname newname))
        (errno-error (errno) create-hard-link oldname newname))))

(define (create-symlink oldname newname . o)
  (let-optionals o ((override? #f))
    (if override? (delete-filesystem-object newname))
    (if (not (%symlink oldname newname))
        (errno-error (errno) create-symlink oldname newname))))

(cond-expand
  (windows
    (define (read-symlink fname) #f))
  (else
    (define (read-symlink fname)
      (let* ((buf (make-string (+ 1 PATH_MAX)))
             (res (%readlink fname buf PATH_MAX)))
        (if (positive? res)
            (substring buf 0 res)
            (errno-error (errno) read-symlink fname))))))

(define (rename-file oldname newname . o)
  (let-optionals o ((override? #f))
    (if (not override?)
        (if (%stat newname)
            (errno-error errno/exist rename-file oldname newname)))
    (if (not (%rename oldname newname))
        (errno-error (errno) rename-file oldname newname))))

(define (delete-directory fname)
  (if (not (%rmdir fname))
      (errno-error (errno) delete-directory fname)))

(define (set-file-mode fname permission-bits)
  (if (not (retry-if-EINTR (lambda () (%chmod fname permission-bits))))
      (errno-error (errno) set-file-mode fname permission-bits)))

(define (set-file-owner fname uid)
  (let ((gid (file-info:gid (file-info fname #t))))
    (if (not (retry-if-EINTR (lambda () (%chown fname uid gid))))
        (errno-error (errno) set-file-owner fname uid gid))))

(define (set-file-group fname gid)
  (let ((uid (file-info:uid (file-info fname #t))))
    (if (not (retry-if-EINTR (lambda () (%chown fname uid gid))))
        (errno-error (errno) set-file-group fname uid gid))))

(define timespec/now (cons -1 utimens/utime_now))
(define timespec/omit (cons -1 utimens/utime_omit))

(define (do-set-file-timespecs fname atime mtime)
  (if (not (%utimensat utimens/at_fdcwd
                       fname
                       ;; don't change underlying representation until timespec SRFI finalized
                       (cons (timespec-seconds atime) (timespec-nanoseconds atime))
                       (cons (timespec-seconds mtime) (timespec-nanoseconds mtime))
                       0))
      (errno-error (errno) set-file-timespecs fname atime mtime)))

(define set-file-timespecs
  (case-lambda
   ((fname) (do-set-file-timespecs fname timespec/now timespec/now))
   ((fname atime mtime) (do-set-file-timespecs fname atime mtime))))

(define (truncate-file fname/port len)
  (cond ((string? fname/port)
         (if (not (retry-if-EINTR (lambda () (%truncate fname/port len))))
             (errno-error (errno) truncate-file fname/port len))) ;; exit the procedure
        ((port? fname/port)
         (if (not (retry-if-EINTR (lambda () (%ftruncate (port-fdes fname/port) len))))
             (errno-error (errno) truncate-file fname/port len))) ;; exit the procedure
        (else (errno-error errno/inval truncate-file fname/port len))))

(cond-expand
  (windows
   (define-record-type File-Info
     (make-file-info device inode mode nlinks uid gid rdev
                     size atime mtime ctime)
     file-info?
     (device file-info:device)
     (inode file-info:inode)
     (mode file-info:mode)
     (nlinks file-info:nlinks)
     (uid file-info:uid)
     (gid file-info:gid)
     (rdev file-info:rdev)
     (size file-info:size)
;;   (blksize file-info:blksize)
;;   (blocks file-info:blocks)
     (atime file-info:atime)
     (mtime file-info:mtime)
     (ctime file-info:ctime)))
  (else
   (define-record-type File-Info
     (make-file-info device inode mode nlinks uid gid rdev
                     size blksize blocks atime mtime ctime)
     file-info?
     (device file-info:device)
     (inode file-info:inode)
     (mode file-info:mode)
     (nlinks file-info:nlinks)
     (uid file-info:uid)
     (gid file-info:gid)
     (rdev file-info:rdev)
     (size file-info:size)
     (blksize file-info:blksize)
     (blocks file-info:blocks)
     (atime file-info:atime)
     (mtime file-info:mtime)
     (ctime file-info:ctime))))

(define (file-info fname/port follow?)
  (let ((file-stat
         (cond ((string? fname/port)
                (let ((the-file-info (if follow?
                                         (%stat fname/port)
                                         (%lstat fname/port))))
                  (if the-file-info
                      the-file-info
                      (errno-error (errno) file-info fname/port)))) ;; exit the procedure
               ((port? fname/port)
                (let ((the-file-info (%fstat (port-fdes fname/port))))
                  (if the-file-info
                      the-file-info
                      (errno-error (errno) file-info fname/port))))))) ;; exit the procedure
    (if (not file-stat)
        (errno-error (errno) file-info fname/port)) ;; exit the procedure
    (make-file-info
     (stat:dev file-stat)
     (stat:ino file-stat)
     (stat:mode file-stat)
     (stat:nlinks file-stat)
     (stat:uid file-stat)
     (stat:gid file-stat)
     (stat:rdev file-stat)
     (stat:size file-stat)
     (stat:blksize file-stat)
     (stat:blocks file-stat)
     (make-timespec (posix-timespec:seconds (stat:atime file-stat)) (posix-timespec:nanoseconds (stat:atime file-stat)))
     (make-timespec (posix-timespec:seconds (stat:mtime file-stat)) (posix-timespec:nanoseconds (stat:mtime file-stat)))
     (make-timespec (posix-timespec:seconds (stat:ctime file-stat)) (posix-timespec:nanoseconds (stat:ctime file-stat))))))

(define (file-info-directory? file-info-record)
  (S_ISDIR (file-info:mode file-info-record)))

(define (file-info-fifo? file-info-record)
  (S_ISFIFO (file-info:mode file-info-record)))

(define (file-info-symlink? file-info-record)
  (S_ISLNK (file-info:mode file-info-record)))

(define (file-info-regular? file-info-record)
  (S_ISREG (file-info:mode file-info-record)))

(define-record-type Directory-Object
  (make-directory-object the-DIR is-open? dot-files?)
  directory-object?
  (the-DIR directory-object-get-DIR)
  (is-open? directory-object-is-open? set-directory-object-is-open)
  (dot-files? directory-object-dot-files?))

;;> Returns a list of the files in \var{dir} in an unspecified
;;> order.

(define (directory-files . o)
  (let-optionals o ((dir (working-directory))
                    (dot-files? #f))
    (directory-fold dir cons '() dot-files?)))

(define make-directory-files-generator
  (case-lambda
    ((dir dot-files?) (make-directory-files-generator* dir dot-files?))
    ((dir) (make-directory-files-generator* dir #f))))

;; Use this commented out version if your dir-obj AKA directory object
;; abstraction doesn't keep track of when it has been closed:

#|
(define (make-directory-files-generator* dir dot-files?)
  (let ((dir-obj (open-directory dir dot-files?))
        (eof (eof-object))
        (done #f))
    (lambda ()
      (if done eof
        (let ((f (read-directory dir-obj)))
          (when (eq? f eof)
            (close-directory dir-obj)
            (set! done #t))
          f)))))
|#

(define (make-directory-files-generator* dir dot-files?)
  (let ((dir-obj (open-directory dir dot-files?))
        (eof (eof-object)))
    (lambda ()
      (if (not (directory-object-is-open? dir-obj))
          eof
          (let ((f (read-directory dir-obj)))
            (if (eq? f eof) (close-directory dir-obj))
            f)))))

(define (open-directory dir . o)
  (let-optionals o ((dot-files? #f))
    (let ((ret (%opendir dir)))
      (if ret
          (make-directory-object ret #t dot-files?)
          (errno-error (errno) open-directory dir)))))

(define (read-directory-raise-error dirobj)
  (set-errno 0)
  (let* ((de (%readdir (directory-object-get-DIR dirobj)))
         (e (errno)))
    (if (equal? 0 e)
        de
        (errno-error e read-directory dirobj))))

(define (read-directory dirobj)
  (if (not (directory-object? dirobj))
      (errno-error errno/inval read-directory dirobj)) ;; exit the procedure
  (if (not (directory-object-is-open? dirobj))
      (errno-error errno/badf read-directory dirobj)) ;; exit the procedure
  (let ((dot-files? (directory-object-dot-files? dirobj)))
    (let loop ()
      (let ((de (read-directory-raise-error dirobj)))
        (if (not de)
            (eof-object)
            (let ((name (dirent-name de)))
              (if (not (and (string? name)
                            (or (equal? "." name)
                                (equal? ".." name)
                                (and (not dot-files?) (equal? #\. (string-ref name 0))))))
                  name
                  (loop))))))))

(define (close-directory directory-object)
  (if (not (directory-object? directory-object))
      (errno-error errno/inval close-directory directory-object)) ;; exit the procedure
  (if (not (directory-object-is-open? directory-object))
      (errno-error errno/badf read-directory directory-object) ;; exit the procedure
      (set-directory-object-is-open directory-object #f)
      ;; does not dirobj any error stuff, see 170.stub
      (%closedir (directory-object-get-DIR directory-object))))

(define (real-path the-starting-path)
  (if (not (string? the-starting-path))
      (errno-error errno/inval real-path the-starting-path)) ;; exit the procedure
  (let ((the-real-path (%realpath the-starting-path)))
    (if the-real-path
        the-real-path
        (errno-error (errno) real-path the-starting-path))))

(define the-character-set "ABCDEFGHIJKLMNOPQURTUVWXYZ0123456789")

(define the-character-set-length (string-length the-character-set))

(define (get-random-character) (string-ref the-character-set (random-integer the-character-set-length)))

(define (suffix-string)
  (string (get-random-character) (get-random-character) (get-random-character)
          (get-random-character) (get-random-character) (get-random-character)
          (get-random-character) (get-random-character) (get-random-character)))

(define temp-file-prefix
  (make-parameter 9
                  (lambda (x) ;; ~~~~ maybe make it the size of the ending string?
                    (let ((the-pair (assoc "TMPDIR" (get-environment-variables)))
                          (the-suffix-string (suffix-string)))
                      (if (pair? the-pair)
                          (string-append (cdr the-pair) "/" (number->string (pid)) "." the-suffix-string)
                          (string-append "/tmp/" (number->string (pid)) "." the-suffix-string))))))

(define (create-temp-file . o)
  (temp-file-prefix #t) ;; ~~~~ brute force if prefix supplied
  (let-optionals o ((prefix (temp-file-prefix)))
    (let loop ()
      (let ((the-filename (string-append prefix "." (suffix-string))))
        (if (file-exists? the-filename)
            (loop)
            (let ((the-fileno (open the-filename (bitwise-ior open/write open/create) #o600)))
              (if (not the-fileno)
                  ;; ~~~~ adding the filename is not in the specs, but necessary for sane debugging
                  (errno-error (errno) create-temp-file prefix the-filename)) ;; exit the procedure
              (retry-if-EINTR (lambda () (%close (%fileno-to-fd the-fileno))))
              the-filename))))))

#|
;; Following copied from scsh 0.7 scheme/temp-file.scm:

(define (temp-file-iterate maker . maybe-template)
  (let ((template (:optional maybe-template (fluid *temp-file-template*))))
    (let loop ((i 0))
      (if (> i 1000) (error "Can't create temp-file")
          (let ((fname (format #f template (number->string i))))
            (receive retvals (with-errno-handler
                               ((errno data)
                                ((exist acces) #f))
                               (maker fname))
              (if (car retvals) (apply values retvals)
                  (loop (+ i 1)))))))))


;; Following 3 defines copied from scsh 0.7 scheme/scsh-condition.scm
;; see e.g. scheme48-1.9.2 for things like os-error?

(define (with-errno-handler* handler thunk)
  (with-exception-handler
    (lambda (condition)
      (if (os-error? condition)
          (handler (os-error-code condition)
                   (list (condition-message condition)
                         (condition-who condition)
                         (condition-irritants condition)))
          (raise condition)))
    thunk))

(define-syntax weh-cond
  (syntax-rules (else)
    ((weh-cond () ((cond-condition cond-body) ...) error-number return)
     (cond (cond-condition cond-body) ...))
    ((weh-cond (((errno-name ...) clause-body ...) . other-clauses) (cond-clause ...) error-number return)
     (weh-cond other-clauses
               (cond-clause ... ((or (errno=? (errno errno-name) (integer->errno error-number)) ...)
                                 (call-with-values (lambda () clause-body ...) return)))
               error-number return))
    ((weh-cond ((else clause-body ...) . other-clauses) (cond-clause ...) error-number return)
     (weh-cond other-clauses
               (cond-clause ... (else (call-with-values (lambda () clause-body ...) return)))
               error-number return))))

(define-syntax with-errno-handler
  (syntax-rules ()
    ((with-errno-handler
      ((err data)
       (clause-condition clause-body ...) ...)
      body ...)
     (call-with-current-continuation
      (lambda (return)
        (with-errno-handler*
         (lambda (err data)
           (weh-cond ((clause-condition clause-body ...) ...) () err return))
         (lambda () body ...)))))))


;; Finishing the following is left as an exercise for the reader

;; Seems to be mostly correct, the with-errno-handler part as the only
;; part definitately not working, and not trivial, see above

(define (call-with-temporary-filename maker . o)
  (if (equal? '() o) (temp-file-prefix #t)) ;; force new prefix if none supplied
  (let-optionals o ((the-prefix (temp-file-prefix)))
    (let loop ((i 0))
      (if (> i 1000) (errno-error errno/inval call-with-temporary-filename maker the-prefix) ;; exit the procedure ~~~~ maybe a better errno (for now)?
          (let ((fname (string-append the-prefix "." (number->string i))))
            (receive retvals (with-errno-handler
                               ((errno data)
                                ((errno/exist errno/acces) #f))
                               (maker fname))
              (if (car retvals) (apply values retvals) ;; ~~~~ don't understand the use of values at all
                  (loop (+ i 1)))))))))
|#


;;; 3.5  Process state

(define (umask)
  (let ((current-umask (%umask #o777)))
    (%umask current-umask)
    current-umask))

(define (set-umask perms)
  (%umask perms))

(define (working-directory)
  (let ((dir (%getcwd)))
    (if (not dir)
      (errno-error (errno) working-directory)
      dir)))

(define (set-working-directory fname)
  (if (not (%chdir fname))
      (errno-error (errno) set-working-directory fname)))

;; pid and parent-pid direct from stub, they can't error

(define (process-group . o)
  (let-optionals o ((process-object/pid 0))
    (let ((pgid (%getpgid process-object/pid)))
      (if (equal? -1 pgid)
          (errno-error (errno) process-group process-object/pid)
          pgid))))

(define (nice . o)
  (let-optionals o ((delta 1))
    (set-errno 0)
    (let ((ret (%nice delta)))
      (if (and (equal? -1 ret) (not (equal? 0 (errno))))
          (errno-error (errno) nice delta)) ;; exit the procedure
      ret)))

(define (user-supplementary-gids)
  (let* ((ret (%getgroups))
         (i (car ret)))
    (if (equal? -1 i)
        (errno-error (errno) user-supplementary-gids)) ;; exit the procedure
    (take (cadr ret) i))) ;; immutable list


;;; 3.6  User and group database access

(define-record-type User-Info
  (make-user-info name uid full-name parsed-full-name gid home-dir shell)
  user-info?
  (name user-info:name)
  (uid user-info:uid)
  (full-name user-info:full-name)
  (parsed-full-name user-info:parsed-full-name)
  (gid user-info:gid)
  (home-dir user-info:home-dir)
  (shell user-info:shell))

(define (user-info user)
  (set-errno 0)
  (let ((ui (if (string? user)
                (retry-if-EINTR (lambda () (%getpwnam user)))
                (retry-if-EINTR (lambda () (%getpwuid user))))))
    (if (not ui)
        (errno-error (errno) user-info user) ;; exit the procedure
        (make-user-info (passwd:name ui)
                        (passwd:uid ui)
                        (passwd:gecos ui)
                        (parse-gecos (passwd:gecos ui) (passwd:name ui))
                        (passwd:gid ui)
                        (passwd:dir ui)
                        (passwd:shell ui)))))

(define-record-type Group-Info
  (make-group-info name gid)
  group-info?
  (name group-info:name)
  (gid group-info:gid))

(define (group-info group)
  (set-errno 0)
  (let ((gi (if (string? group)
                (retry-if-EINTR (lambda () (%getgrnam group)))
                (retry-if-EINTR (lambda () (%getgrgid group))))))
    (if (not gi)
        (errno-error (errno) group-info group) ;; exit the procedure
        (make-group-info (group:name gi)
                         (group:gid gi)))))


;;; 3.10  Time

(define (posix-time)
  (let ((t (%clock_gettime clck-id/realtime)))
    (if (not t)
        (errno-error (errno) posix-time)
        (make-timespec (posix-timespec:seconds t) (posix-timespec:nanoseconds t)))))

(define (monotonic-time)
  (let ((t (%clock_gettime clck-id/monotonic)))
    (if (not t)
        (errno-error (errno) monotonic-time)
        (make-timespec (posix-timespec:seconds t) (posix-timespec:nanoseconds t)))))


;;; 3.12  Terminal device control

(define (terminal? the-port)
  (if (not (port? the-port))
      (errno-error errno/inval terminal? the-port)) ;; exit the procedure
  (let ((the-fd (port-fdes the-port)))
    (if (not the-fd)
        #f)
    (begin
      (set-errno 0)
      (let ((ret (%isatty the-fd)))
        (if (equal? 1 ret)
            #t
            (if (or (not (equal? 0 ret))
                    (not (equal? errno/notty (errno))))
                (errno-error (errno) terminal? the-port) ;; exit the procedure
                #f))))))

(define (terminal-file-name the-port) ;; ~~~~ add fd??
  (if (not (port? the-port))
      (errno-error errno/inval terminal-file-name the-port)) ;; exit the procedure
  (let ((the-fd (port-fdes the-port)))
    (if (not the-fd)
        (errno-error errno/inval terminal-file-name the-port)) ;; exit the procedure
    (let ((the-file-name (%ttyname_r the-fd)))
      (if (not the-file-name)
          (errno-error (errno) terminal-file-name the-port)) ;; exit the procedure
      the-file-name)))


;; ~~~~ all prefactory with- and without- errno-errors need a more
;;      specific error indicator, that's why they're not combined

(define (with-raw-mode input-port output-port min time proc)
  (if (not (and (port? input-port) (port? output-port)))
      (errno-error errno/inval with-raw-mode input-port output-port min time proc)) ;; exit the procedure
  (if (not (and (terminal? input-port) (terminal? output-port)))
      (errno-error errno/inval with-raw-mode input-port output-port min time proc)) ;; exit the procedure
  (if (not (and (input-port? input-port) (output-port? output-port)))
      (errno-error errno/inval with-raw-mode input-port output-port min time proc)) ;; exit the procedure
  (if (not (exact-integer? min))
      (errno-error errno/inval with-raw-mode input-port output-port min time proc)) ;; exit the procedure
  (if (not (exact-integer? time))
      (errno-error errno/inval with-raw-mode input-port output-port min time proc)) ;; exit the procedure

  (let* ((initial-input-termios (%tcgetattr input-port))
         (initial-output-termios (%tcgetattr output-port))
         (new-input-termios (%tcgetattr input-port)) ;; ~~~~ because of tagging, how to copy is not obvious
         (new-output-termios (%tcgetattr output-port)) ;; ~~~~ because of tagging, how to copy is not obvious
         (reset-terminal (lambda ()
                           (let ((input-return (retry-if-EINTR (lambda () (%tcsetattr input-port TCSAFLUSH initial-input-termios))))) ;; still try resetting output
                             (if (not (and (retry-if-EINTR (lambda () (%tcsetattr output-port TCSAFLUSH initial-output-termios))) input-return))
                                 (errno-error (errno) with-raw-mode input-port output-port min time proc))))) ;; might as well exit the procedure
         ;; ~~~~~~~ set all for *both* ports???
         (the-lflags (bitwise-ior ECHO ICANON IEXTEN ISIG))
         (the-iflags (bitwise-ior BRKINT ICRNL INPCK ISTRIP IXON))
         (the-and-cflags (bitwise-ior CSIZE PARENB))
         (the-ior-cflags CS8)
         (the-oflags OPOST))

    (if (or (not initial-input-termios) (not new-input-termios) (not initial-output-termios) (not new-output-termios))
        (errno-error (errno) with-raw-mode input-port output-port min time proc)) ;; exit the procedure

    (term-attrs-lflag-set! new-input-termios
                           (bitwise-and (term-attrs-lflag new-input-termios) (bitwise-not the-lflags)))
    (term-attrs-iflag-set! new-input-termios
                           (bitwise-and (term-attrs-iflag new-input-termios) (bitwise-not the-iflags)))
    (term-attrs-cflag-set! new-input-termios
                           (bitwise-and (term-attrs-cflag new-input-termios) (bitwise-not the-and-cflags)))
    (term-attrs-cflag-set! new-input-termios
                           (bitwise-ior (term-attrs-cflag new-input-termios) the-ior-cflags))
    (term-attrs-oflag-set! new-input-termios
                           (bitwise-and (term-attrs-oflag new-input-termios) (bitwise-not the-oflags)))
    (term-attrs-cc-element-set! new-input-termios min VMIN) ;; ~~~~ ought to transpose array index and value to put in it
    (term-attrs-cc-element-set! new-input-termios time VTIME) ;; ~~~~ ought to transpose array index and value to put in it

    (term-attrs-lflag-set! new-output-termios
                           (bitwise-and (term-attrs-lflag new-output-termios) (bitwise-not the-lflags)))
    (term-attrs-iflag-set! new-output-termios
                           (bitwise-and (term-attrs-iflag new-output-termios) (bitwise-not the-iflags)))
    (term-attrs-cflag-set! new-output-termios
                           (bitwise-and (term-attrs-cflag new-output-termios) (bitwise-not the-and-cflags)))
    (term-attrs-cflag-set! new-output-termios
                           (bitwise-ior (term-attrs-cflag new-output-termios) the-ior-cflags))
    (term-attrs-oflag-set! new-output-termios
                           (bitwise-and (term-attrs-oflag new-output-termios) (bitwise-not the-oflags)))
    (dynamic-wind
        (lambda ()      ;; set output port first since input port is the same + VMIN and VTIME, we're probably doing duplicate tcsetattrs at the OS level
          (if (not (and (retry-if-EINTR (lambda () (%tcsetattr output-port TCSAFLUSH new-output-termios)))
                        (retry-if-EINTR (lambda () (%tcsetattr input-port TCSAFLUSH new-input-termios)))))
              (errno-error (errno) with-raw-mode input-port output-port min time proc) ;; exit the procedure

              ;; For historical reasons, tcsetattr returns 0 if *any*
              ;; of the attribute changes took, so we must check to
              ;; see if all have been set
              (let ((real-new-input-termios (%tcgetattr input-port))
                    (real-new-output-termios (%tcgetattr output-port)))
                (if (not (and real-new-input-termios real-new-output-termios))
                    (begin (reset-terminal)
                           (errno-error (errno) with-raw-mode input-port output-port min time proc)) ;; exit the procedure
                    (if (not (and (equal? (term-attrs-lflag new-input-termios) (term-attrs-lflag real-new-input-termios))
                                  (equal? (term-attrs-iflag new-input-termios) (term-attrs-iflag real-new-input-termios))
                                  (equal? (term-attrs-cflag new-input-termios) (term-attrs-cflag real-new-input-termios))
                                  (equal? (term-attrs-oflag new-input-termios) (term-attrs-oflag real-new-input-termios))
                                  (equal? min (term-attrs-cc-element real-new-input-termios VMIN))
                                  (equal? time (term-attrs-cc-element real-new-input-termios VTIME))

                                  (equal? (term-attrs-lflag new-output-termios) (term-attrs-lflag real-new-output-termios))
                                  (equal? (term-attrs-iflag new-output-termios) (term-attrs-iflag real-new-output-termios))
                                  (equal? (term-attrs-cflag new-output-termios) (term-attrs-cflag real-new-output-termios))
                                  (equal? (term-attrs-oflag new-output-termios) (term-attrs-oflag real-new-output-termios))))
                        (begin (reset-terminal)
                               (errno-error errno/inval with-raw-mode input-port output-port min time proc))))))) ;; exit the procedure
        (lambda () (proc input-port output-port))
        (lambda ()
          (reset-terminal)))))

(define (with-rare-mode input-port output-port proc)
  (if (not (and (port? input-port) (port? output-port)))
      (errno-error errno/inval with-rare-mode input-port output-port proc)) ;; exit the procedure
  (if (not (and (terminal? input-port) (terminal? output-port)))
      (errno-error errno/inval with-rare-mode input-port output-port proc)) ;; exit the procedure
  (if (not (and (input-port? input-port) (output-port? output-port)))
      (errno-error errno/inval with-rare-mode input-port output-port proc)) ;; exit the procedure

  (let* ((initial-input-termios (%tcgetattr input-port))
         (initial-output-termios (%tcgetattr output-port))
         (new-input-termios (%tcgetattr input-port)) ;; ~~~~ because of tagging, how to copy is not obvious
         (new-output-termios (%tcgetattr output-port)) ;; ~~~~ because of tagging, how to copy is not obvious
         (reset-terminal (lambda ()
                           (let ((input-return (retry-if-EINTR (lambda () (%tcsetattr input-port TCSAFLUSH initial-input-termios))))) ;; still try resetting output
                             (if (not (and (retry-if-EINTR (lambda () (%tcsetattr output-port TCSAFLUSH initial-output-termios))) input-return))
                                 (errno-error (errno) with-rare-mode input-port output-port proc))))) ;; might as well exit the procedure
         (the-lflags (bitwise-ior ICANON ECHO))) ;; ~~~~~~~ set for *both* ports???

    (if (or (not initial-input-termios) (not new-input-termios) (not initial-output-termios) (not new-output-termios))
        (errno-error (errno) with-rare-mode input-port output-port proc)) ;; exit the procedure

    (term-attrs-lflag-set! new-input-termios
                           (bitwise-and (term-attrs-lflag new-input-termios) (bitwise-not the-lflags)))
    (term-attrs-cc-element-set! new-input-termios 1 VMIN) ;; ~~~~ ought to transpose array index and value to put in it
    (term-attrs-cc-element-set! new-input-termios 0 VTIME) ;; ~~~~ ought to transpose array index and value to put in it
    (term-attrs-lflag-set! new-output-termios
                           (bitwise-and (term-attrs-lflag new-output-termios) (bitwise-not the-lflags)))
    (dynamic-wind
        (lambda ()      ;; set output port first since input port is the same + VMIN and VTIME, we're probably doing duplicate tcsetattrs at the OS level
          (if (not (and (retry-if-EINTR (lambda () (%tcsetattr output-port TCSAFLUSH new-output-termios)))
                        (retry-if-EINTR (lambda () (%tcsetattr input-port TCSAFLUSH new-input-termios)))))
              (errno-error (errno) with-rare-mode input-port output-port proc) ;; exit the procedure

              ;; For historical reasons, tcsetattr returns 0 if *any*
              ;; of the attribute changes took, so we must check to
              ;; see if all have been set
              (let ((real-new-input-termios (%tcgetattr input-port))
                    (real-new-output-termios (%tcgetattr output-port)))
                (if (not (and real-new-input-termios real-new-output-termios))
                    (begin (reset-terminal)
                           (errno-error (errno) with-rare-mode input-port output-port proc)) ;; exit the procedure
                    (if (not (and (equal? 0 (bitwise-and (term-attrs-lflag real-new-input-termios) the-lflags))
                                  (equal? 1 (term-attrs-cc-element real-new-input-termios VMIN))
                                  (equal? 0 (term-attrs-cc-element real-new-input-termios VTIME))
                                  (equal? 0 (bitwise-and (term-attrs-lflag real-new-output-termios) the-lflags))))
                        (begin (reset-terminal)
                               (errno-error errno/inval with-rare-mode input-port output-port proc))))))) ;; exit the procedure
        (lambda () (proc input-port output-port))
        (lambda ()
          (reset-terminal)))))

(define (without-echo input-port output-port proc)
  (if (not (and (port? input-port) (port? output-port)))
      (errno-error errno/inval without-echo input-port output-port proc)) ;; exit the procedure
  (if (not (and (terminal? input-port) (terminal? output-port)))
      (errno-error errno/inval without-echo input-port output-port proc)) ;; exit the procedure
  (if (not (and (input-port? input-port) (output-port? output-port)))
      (errno-error errno/inval without-echo input-port output-port proc)) ;; exit the procedure

  (let* ((initial-output-termios (%tcgetattr output-port))
         (new-output-termios (%tcgetattr output-port)) ;; ~~~~ because of tagging, how to copy is not obvious
         (reset-terminal (lambda ()
                           (if (not (retry-if-EINTR (lambda () (%tcsetattr output-port TCSAFLUSH initial-output-termios))))
                               (errno-error (errno) without-echo output-port proc)))) ;; might as well exit the procedure
         (the-lflags (bitwise-ior ECHO ECHOE ECHOK ECHONL)))

    (if (or (not initial-output-termios) (not new-output-termios))
        (errno-error (errno) without-echo output-port proc)) ;; exit the procedure
    (term-attrs-lflag-set! new-output-termios
                           (bitwise-and (term-attrs-lflag new-output-termios) (bitwise-not the-lflags)))
    (dynamic-wind
        (lambda ()
          (if (not (retry-if-EINTR (lambda () (%tcsetattr output-port TCSAFLUSH new-output-termios))))
              (errno-error (errno) without-echo output-port proc) ;; exit the procedure

              ;; For historical reasons, tcsetattr returns 0 if *any*
              ;; of the attribute changes took, so we must check to
              ;; see if all have been set
              (let ((real-new-output-termios (%tcgetattr output-port)))
                (if (not real-new-output-termios)
                    (begin (reset-terminal)
                           (errno-error (errno) without-echo output-port proc)) ;; exit the procedure
                    (if (not (equal? 0 (bitwise-and (term-attrs-lflag real-new-output-termios) the-lflags)))
                        (begin (reset-terminal)
                               (errno-error errno/inval without-echo output-port proc))))))) ;; exit the procedure
        (lambda () (proc input-port output-port))
        (lambda ()
          (reset-terminal)))))
