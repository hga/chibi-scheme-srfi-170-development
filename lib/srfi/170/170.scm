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



(define (port-fdes the-port)
  (if (not (port? the-port))
      (errno-error errno/inval port-fdes the-port))
  (port-fileno the-port))

(define (close-fdes the-fd)
  (if (or (not (fixnum? the-fd)) (< the-fd 0))
      (errno-error errno/inval close-fdes the-fd))
  (if (not (%close the-fd))
      (errno-error (errno) close-fdes the-fd)))


;;; 3.3  File system

(define (create-directory fname . o)
  (let-optionals o ((permission-bits #o775)
                    (override? #f))
    (if override? (delete-filesystem-object fname))
    (if (not (%create-directory fname permission-bits))
        (errno-error (errno) create-directory fname))))

(define (create-fifo fname . o)
  (let-optionals o ((permission-bits #o664)
                    (override? #f))
    (if override? (delete-filesystem-object fname))
    (if (not (%create-fifo fname permission-bits))
        (errno-error (errno) create-fifo fname))))

(define (create-hard-link oldname newname . o)
  (let-optionals o ((override? #f))
    (if override? (delete-filesystem-object newname))
    (if (not (%create-hard-link oldname newname))
        (errno-error (errno) create-hard-link oldname newname))))

(define (create-symlink oldname newname . o)
  (let-optionals o ((override? #f))
    (if override? (delete-filesystem-object newname))
    (if (not (%create-symlink oldname newname))
        (errno-error (errno) create-symlink oldname newname))))

(define (rename-file oldname newname . o)
  (let-optionals o ((override? #f))
    (if (not override?)
        (if (%lstat newname)
            (errno-error errno/exist rename-file oldname newname)))
    (if (not (%rename-file oldname newname))
        (errno-error (errno) rename-file oldname newname))))

(define (delete-directory fname)
  (if (not (%delete-directory fname))
      (errno-error (errno) delete-directory fname)))

(define (set-file-mode fname/port permission-bits)
  (if (not (%set-file-mode fname/port permission-bits))
      (errno-error (errno) set-file-mode fname/port permission-bits)))

(define (set-file-owner fname/port uid . o)
  (let-optionals o ((chase? #t))
    (let ((gid (file-info:gid (file-info fname/port))))
      (if chase?
          (if (not (%chown fname/port uid gid))
              (errno-error (errno) set-file-owner fname/port uid gid))
          (if (not (%lchown fname/port uid gid))
              (errno-error (errno) set-file-owner fname/port uid gid))))))

(define (set-file-group fname/port gid . o)
  (let-optionals o ((chase? #t))
    (let ((uid (file-info:uid (file-info fname/port))))
      (if chase?
          (if (not (%chown fname/port uid gid))
              (errno-error (errno) set-file-group fname/port uid gid))
          (if (not (%lchown fname/port uid gid))
              (errno-error (errno) set-file-group fname/port uid gid))))))

(define timespect/now (cons -1 utimens/utime_now))
(define timespec/omit (cons -1 utimens/utime_omit))

(define (do-set-file-timespecs fname/port atime mtime chase?)
  (if (not (%utimensat utimens/at_fdcwd fname/port atime mtime (if chase?
                                                                   0
                                                                   utimens/at_symlink_nofollow)))
      (errno-error (errno) set-file-timespecs fname/port atime mtime chase?)))

(define set-file-timespecs
  (case-lambda
   ((fname/port) (do-set-file-timespecs fname/port timespect/now timespect/now #t))
   ((fname/port chase?) (do-set-file-timespecs fname/port timespect/now timespect/now chase?))
   ((fname/port atime mtime) (do-set-file-timespecs fname/port atime mtime #t))
   ((fname/port atime mtime chase?) (do-set-file-timespecs fname/port atime mtime chase?))))

(define (truncate-file fname/port len)
  (if (not (%truncate fname/port len))
      (errno-error (errno) truncate-file fname/port len)))

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

(define (file-info fname/port . o)
  (let-optionals o ((chase? #t))
    (let ((file-stat (if chase?
                     (%stat fname/port)
                     (%lstat fname/port))))
      (if (not file-stat)
          (errno-error (errno) file-info fname/port)) ;; non-local exit
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
       (cons (timespec:seconds (stat:ctime file-stat)) (timespec:nanoseconds (stat:ctime file-stat)))))))


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

(define (file-info-socket? file-info-record)
  (if (eq? 0 (bitwise-and file-type-mask/ifsock (file-info:mode file-info-record))) #f #t))

(define (file-info-block-special? file-info-record)
  (if (eq? 0 (bitwise-and file-type-mask/ifblk (file-info:mode file-info-record))) #f #t))

(define (file-info-character-special? file-info-record)
  (if (eq? 0 (bitwise-and file-type-mask/ifchr (file-info:mode file-info-record))) #f #t))

(define (file-info-symlink? file-info-record)
  (if (eq? 0 (bitwise-and file-type-mask/iflnk (file-info:mode file-info-record))) #f #t))

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
      (errno-error errno/inval read-directory dirobj)) ;; non-local exit
  (if (not (directory-object-is-open? dirobj))
      (errno-error errno/badf read-directory dirobj)) ;; non-local exit
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
      (errno-error errno/inval close-directory directory-object)) ;; non-local exit
  (if (not (directory-object-is-open? directory-object))
      (errno-error errno/badf read-directory directory-object) ;; non-local exit
      (set-directory-object-is-open directory-object #f)
      ;; does not dirobj any error stuff, see 170.stub
      (%closedir (directory-object-get-DIR directory-object))))



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

(define (working-directory)
  (let ((dir (%getcwd)))
    (if (not dir)
      (errno-error (errno) working-directory)
      dir)))

(define (set-working-directory fname)
  (if (not (%chdir fname))
      (errno-error (errno) set-working-directory fname)))

;; pid and parent-pid direct from stub, can't error

(define (process-group . o)
  (let-optionals o ((process-object/pid 0))
    (let ((pgid (%getpgid process-object/pid)))
      (if (equal? -1 pgid)
          (errno-error (errno) process-group process-object/pid)
          pgid))))

(define set-process-group
  (case-lambda
   ((pgrp)
    (if (not (%setpgid 0 pgrp))
        (errno-error (errno) set-process-group pgrp)))
   ((process-object/pid pgrp)
    (if (not (%setpgid process-object/pid pgrp))
        (errno-error (errno) set-process-group process-object/pid pgrp)))))

(define (priority which who)
  ;; ~~~~~~~~ as a proxy for setting errno to zero, set it to a known
  ;; value with an operation that will fail reliably
  (%create-directory "/" #o755)
  (let ((niceness (%getpriority which who)))
    (if (equal? -1 niceness)
        (let ((e (errno))) ;; using the above errno proxy
          (if (or (equal? e errno/srch) (equal? e errno/inval))
              (errno-error (errno) priority which who)))) ;; non-local exit
    niceness))

(define (set-priority which who niceness)
  (if (not (%setpriority which who niceness))
      (errno-error (errno) set-priority which who niceness)))

(define (nice . o)
  (let-optionals o ((process-object/pid (pid))
                    (delta 1))
    (set-priority priority/process
                  process-object/pid
                  (+ (priority priority/process process-object/pid) delta))))

(define (user-login-name)
  (let ((name (%getlogin_r)))
    (if (not name)
        (errno-error (errno) user-login-name))
    name))

(define (user-supplementary-gids)
  (let* ((ret (%getgroups))
         (i (car ret)))
    (if (equal? -1 i)
        (errno-error (errno) user-supplementary-gids)) ;; non-local exit
    (take (cadr ret) i))) ;; immutable list

(define (set-uid uid)
  (if (not (%setuid uid))
      (errno-error (errno) set-uid uid)))

(define (set-gid gid)
  (if (not (%setgid gid))
      (errno-error (errno) set-gid gid)))

(define (set-user-effective-uid uid)
  (if (not (%seteuid uid))
      (errno-error (errno) set-user-effective-uid uid)))

(define (set-user-effective-gid gid)
  (if (not (%setegid gid))
      (errno-error (errno) set-user-effective-gid gid)))

(define (set-user-real-and-effective-uid ruid euid)
  (if (not (%setreuid ruid euid))
      (errno-error (errno) set-user-real-and-effective-uid ruid euid)))

(define (set-user-real-and-effective-gid rgid egid)
  (if (not (%setregid rgid egid))
      (errno-error (errno) set-user-real-and-effective-gid rgid egid)))


;;; 3.6  User and group database access

(define-record-type User-Info
  (make-user-info name uid gid home-dir shell)
  user-info?
  (name user-info:name)
  (uid user-info:uid)
  (gid user-info:gid)
  (home-dir user-info:home-dir)
  (shell user-info:shell))

(cond-expand
 (bsd
  (define (user-info user)
    (let ((ui (car (if (string? user)
                       (%getpwnam_r user (make-string 1024))
                       (%getpwuid_r user (make-string 1024))))))
      (if (not (passwd:name ui))
          (errno-error (errno) user-info user) ;; non-local exit
          (make-user-info
           (passwd:name ui)
           (passwd:uid ui)
           (passwd:gid ui)
           (passwd:dir ui)
           (passwd:shell ui))))))
 (else
  ;; Bionic Beaver does not report error
  (define (user-info user)
    (let ((ui (car (if (string? user)
                       (%getpwnam_r user (make-string 1024))
                       (%getpwuid_r user (make-string 1024))))))
      (make-user-info
       (passwd:name ui)
       (passwd:uid ui)
       (passwd:gid ui)
       (passwd:dir ui)
       (passwd:shell ui))))))

(cond-expand
 ((not bsd)
  (define-record-type Group-Info
    (make-group-info name gid)
    group-info?
    (name group-info:name)
    (gid group-info:gid))

  ;; Bionic Beaver does not report error, OpenBSD 6.5 always returns #f
  (define (group-info group)
    (let ((gi (car (if (string? group)
                       (%getgrnam_r group (make-string 1024))
                       (%getgrgid_r group (make-string 1024))))))
      (make-group-info
       (group:name gi)
       (group:gid gi))))
  ))


;;; 3.7  [Intentionally omitted]

;;; 3.8  System parameters

(define (system-name)
  (let ((name (%gethostname)))
    (if (not name)
        (errno-error (errno) system-name))
    name))

(define (uname)
  (let* ((r (%uname))
         (ret (car r))
         (un (cadr r)))
    (if (> ret -1)
        un
        (errno-error (errno) uname))))



;;; 3.9  Signal system

(define (signal-process proc sig)
  (if (not (%kill proc sig))
      ((errno-error (errno) signal-process proc sig))))

(define (signal-process-group prgrp sig)
  (if (not (%killpg prgrp sig))
      ((errno-error (errno) signal-process-group prgrp sig))))


;;; 3.10  Time

(define (posix-time)
  (let ((t (%clock-gettime clck-id/realtime)))
    (if (not t)
        (errno-error (errno) posix-time)
        (cons (timespec:seconds t) (timespec:nanoseconds t)))))

(define (monotonic-time)
  (let ((t (%clock-gettime clck-id/monotonic)))
    (if (not t)
        (errno-error (errno) monotonic-time)
        (cons (timespec:seconds t) (timespec:nanoseconds t)))))

(define (timespec-difference timespec1 timespec2)
  (cons (- (car timespec1) (car timespec2)) (- (cdr timespec1) (cdr timespec2))))


;;; 3.11  [Intentionally omitted]


;;; 3.12  Terminal device control
