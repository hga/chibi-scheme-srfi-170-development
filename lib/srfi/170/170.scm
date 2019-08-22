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
  (let ((gid (file-info:gid (file-info fname))))
    (if (not (retry-if-EINTR (lambda () (%chown fname uid gid))))
        (errno-error (errno) set-file-owner fname uid gid))))

(define (set-file-group fname gid)
  (let ((uid (file-info:uid (file-info fname))))
    (if (not (retry-if-EINTR (lambda () (%chown fname uid gid))))
        (errno-error (errno) set-file-group fname uid gid))))

(define timespect/now (cons -1 utimens/utime_now))
(define timespec/omit (cons -1 utimens/utime_omit))

(define (do-set-file-timespecs fname atime mtime)
  (if (not (%utimensat utimens/at_fdcwd fname atime mtime 0))
           (errno-error (errno) set-file-timespecs fname atime mtime)))

(define set-file-timespecs
  (case-lambda
   ((fname) (do-set-file-timespecs fname timespect/now timespect/now))
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

(define (file-info fname/port)
  (let ((file-stat
         (cond ((string? fname/port)
                (let ((the-file-info (%stat fname/port)))
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
     (cons (timespec:seconds (stat:atime file-stat)) (timespec:nanoseconds (stat:atime file-stat)))
     (cons (timespec:seconds (stat:mtime file-stat)) (timespec:nanoseconds (stat:mtime file-stat)))
     (cons (timespec:seconds (stat:ctime file-stat)) (timespec:nanoseconds (stat:ctime file-stat))))))


#| ~~~~~~~~
;;> File status accessors.  \var{x} should be a string indicating
;;> the file to lookup the status for, or an existing status object.
;;> Raises an error in the string case for non-existing files.
;;/

(define-syntax file-test-mode
  (syntax-rules ()
    ((file-test-mode op x)
     (let* ((tmp x)
            (st (if (stat? tmp) tmp (file-status tmp))))
       (and st (op (stat-mode st)))))))

(define (file-regular? x) (file-test-mode S_ISREG x))
(define (file-directory? x) (file-test-mode S_ISDIR x))
(define (file-character? x) (file-test-mode S_ISCHR x))
(define (file-block? x) (file-test-mode S_ISBLK x))
(define (file-fifo? x) (file-test-mode S_ISFIFO x))
(cond-expand
  (windows
    (define (file-link? x) #f))
  (else
    (define (file-link? x)
      (let ((st (if (stat? x) x (file-link-status x))))
       (and st (S_ISLNK (stat-mode st)))))))
(define (file-socket? x) (file-test-mode S_ISSOCK x))
(define (file-exists? x) (and (if (stat? x) #t (file-status x)) #t))
|#



(define (file-info-directory? file-info-record)
  (if (eq? 0 (bitwise-and file-type-mask/ifdir (file-info:mode file-info-record))) #f #t))

(define (file-info-fifo? file-info-record)
  (if (eq? 0 (bitwise-and file-type-mask/ififo (file-info:mode file-info-record))) #f #t))

(define (file-info-regular? file-info-record)
  (if (eq? 0 (bitwise-and file-type-mask/ifreg (file-info:mode file-info-record))) #f #t))

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
;; Original version from scsh 0.7
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


(define (temp-file-iterate maker . o)
  (temp-file-prefix #t) ;; ~~~~ brute force if prefix supplied
  (let-optionals o ((the-prefix (temp-file-prefix)))
    (let loop ((i 0))
      (if (> i 1000)
          (errno-error errno/inval temp-file-iterate maker the-prefix) ;; exit the procedure
          (let ((fname (string-append the-prefix "." (number->string i))))
;; rest left as an exercise for someone else ^_^
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
  (make-user-info name uid gecos gid home-dir shell)
  user-info?
  (name user-info:name)
  (uid user-info:uid)
  (gecos user-info:gecos)
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
        (cons (timespec:seconds t) (timespec:nanoseconds t)))))

(define (monotonic-time)
  (let ((t (%clock_gettime clck-id/monotonic)))
    (if (not t)
        (errno-error (errno) monotonic-time)
        (cons (timespec:seconds t) (timespec:nanoseconds t)))))


;;; 3.12  Terminal device control

(define (tty? the-port)
  (if (not (port? the-port))
      (errno-error errno/inval tty? the-port)) ;; exit the procedure
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
                (errno-error (errno) tty? the-port) ;; exit the procedure
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

;; rare/cbreak mode is -ICANON -ECHO VMIN=1 VTIME=0

;; without-echo is -ECHO

;; ~~~~ all prefactory errno-errors need a more specific error indicator
(define (without-echo output-port proc)
  (if (not (port? output-port))
      (errno-error errno/inval without-echo output-port proc)) ;; exit the procedure
  (if (not (tty? output-port))
      (errno-error errno/inval without-echo output-port proc)) ;; exit the procedure
  (if (not (output-port? output-port))
      (errno-error errno/inval without-echo output-port proc)) ;; exit the procedure
  (let ((the-fd (port-fdes output-port)))
    (if (not the-fd)
        (errno-error errno/inval without-echo output-port proc)) ;; exit the procedure
    (dynamic-wind
        (lambda ()
          'something-for-body)
        (lambda () (proc output-port))
        (lambda ()
          'something-for-body)
      )
    )
)
