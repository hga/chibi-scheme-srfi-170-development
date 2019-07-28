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

;; Helper function; unlike scsh version, will raise an exception if
;; there is a object it can't delete.  If no object, no exception.

(define (delete-filesystem-object fname)
  (if (%lstat fname)
      (if (file-info-directory? (file-info fname #f))
          (if (not (%delete-directory fname))
              (errno-error (errno) delete-filesystem-object fname))
          (if (not (%delete-file fname))
              (errno-error (errno) delete-filesystem-object fname)))))

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

;; ~~~~ need FFI for set-file-timespecs, see stub file

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
       (cons (%timespec:seconds (stat:atime file-stat)) (%timespec:nanoseconds (stat:atime file-stat)))
       (cons (%timespec:seconds (stat:mtime file-stat)) (%timespec:nanoseconds (stat:mtime file-stat)))
       (cons (%timespec:seconds (stat:ctime file-stat)) (%timespec:nanoseconds (stat:ctime file-stat)))))))

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


;;; ----------------

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

  ;; Be sure to raise exception if wrong clock!



;;; 3.11  Environment variables

;;; 3.12  Terminal device control
