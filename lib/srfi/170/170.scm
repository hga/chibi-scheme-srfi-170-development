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



(define (delete-directory fname)
  (if (not (%delete-directory fname))
      (errno-error (errno) delete-directory fname)))

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
       (cons (stat:atime file-stat) 0)
       (cons (stat:mtime file-stat) 0)
       (cons (stat:ctime file-stat) 0)))))

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

;;; 3.6  User and group database access

;;; 3.7  [Intentionally omitted]

;;; 3.8  System parameters

;;; 3.9  Signal system

;;; 3.10  Time

  ;; Be sure to raise exception if wrong clock!



;;; 3.11  Environment variables

;;; 3.12  Terminal device control
