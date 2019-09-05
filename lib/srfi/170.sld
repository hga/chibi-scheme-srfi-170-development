
(define-library (srfi 170)
  (export

;;; TMP for debugging purposes ----------------
#|
   errno set-errno integer->error-string

   %fileno-to-fd
   %file_descriptor_to_port
   %close

   %rmdir %delete-file

   %stat stat:atime stat:mtime stat:ctime

   parse-gecos

   PATH_MAX

   %opendir %readdir %closedir
   DIR? dirent-name make-directory-object directory-object?
   directory-object-get-DIR directory-object-is-open? set-directory-object-is-open directory-object-dot-files?

   utimens/utime_now utimens/utime_omit
   %utimensat

   posix/path-max

   %realpath

   groups/max %getgroups

   passwd:name passwd:uid
   %getpwuid %getpwnam

   group:name group:gid
   %getgrgid %getgrnam


   timespec:seconds timespec:nanoseconds
   clck-id/realtime clck-id/monotonic
   %clock_gettime

   NCCS ;; size of c_cc array in termios
   %tcgetattr
   %tcsetattr
   term-attrs-cc-element term-attrs-cc-element-set!
|#

   ;; 3.1  Errors COMPLETE

   errno/2big errno/acces errno/addrinuse errno/addrnotavail
   errno/afnosupport errno/again errno/already errno/badf errno/badmsg
   errno/busy errno/canceled errno/child errno/connaborted
   errno/connrefused errno/connreset errno/deadlk errno/destaddrreq
   errno/dom errno/dquot errno/exist errno/fault errno/fbig
   errno/hostunreach errno/idrm errno/ilseq errno/inprogress
   errno/intr errno/inval errno/io errno/isconn errno/isdir errno/loop
   errno/mfile errno/mlink errno/msgsize errno/nametoolong
   errno/netdown errno/netreset errno/netunreach errno/nfile
   errno/nobufs errno/nodev errno/noent errno/noexec errno/nolck
   errno/nomem errno/nomsg errno/noprotoopt errno/nospc errno/nosys
   errno/notconn errno/notdir errno/notempty errno/notrecoverable
   errno/notsock errno/notsup errno/notty errno/nxio errno/opnotsupp
   errno/overflow errno/ownerdead errno/perm errno/pipe errno/proto
   errno/protonosupport errno/prototype errno/range errno/rofs
   errno/spipe errno/srch errno/stale errno/timedout errno/txtbsy
   errno/wouldblock errno/xdev

   errno-error syscall-error?
   syscall-error:errno syscall-error:message
   syscall-error:procedure syscall-error:data
   

   ;; 3.2  I/O COMPLETE

   fdes->textual-input-port fdes->binary-input-port
   fdes->textual-output-port fdes->binary-output-port
   port-fdes
   close-fdes


   ;; 3.3  File system COMPLETE except call-with-temporary-filename

   create-directory create-fifo create-hard-link create-symlink
   read-symlink
   rename-file
   delete-directory
   set-file-mode set-file-owner set-file-group
   set-file-timespecs timespect/now timespec/omit
   truncate-file

   file-info file-info?
   file-info:device file-info:inode file-info:mode file-info:nlinks
   file-info:uid file-info:gid file-info:rdev file-info:size
   file-info:atime file-info:mtime file-info:ctime

   file-info-directory? file-info-fifo? file-info-symlink? file-info-regular?

   directory-files
   open-directory read-directory close-directory

   real-path

   temp-file-prefix
   create-temp-file
   ;; call-with-temporary-filename


   ;; 3.5  Process state COMPLETE

   umask set-umask
   working-directory set-working-directory
   pid parent-pid process-group
   nice

   user-uid user-gid
   user-effective-uid user-effective-gid
   user-supplementary-gids


   ;; 3.6  User and group database access

   user-info user-info?
   user-info:name user-info:uid user-info:gid user-info:home-dir user-info:shell
   user-info:full-name user-info:parsed-full-name

   group-info group-info?
   group-info:name group-info:gid


   ;; 3.10  Time COMPLETE

   posix-time monotonic-time


   ;; 3.12  Terminal device control

   terminal?
   terminal-file-name
   with-raw-mode with-rare-mode without-echo

   )
  
  (cond-expand ((not bsd)
    (export

     ;; 3.1  Errors

     errno/multihop errno/nolink
     ;; STREAMS:
     errno/nodata errno/nostr errno/nosr errno/time

    )))

  (cond-expand ((not windows)
    (export

     ;; 3.3  File system

     file-info:blksize file-info:blocks)))

  (cond-expand
   (chibi
    (import (scheme base)
            (scheme case-lambda)
            (only (scheme process-context) get-environment-variable)
            (chibi)
            (chibi optional) ;; Snow package for optional args
            (only (chibi filesystem) file-exists? delete-file open open/write open/create)
            (only (srfi 1) take)
            (only (srfi 27) random-integer)
            (only (srfi 98) get-environment-variables)
            (only (srfi 115) regexp-replace-all regexp-split)
            (srfi 151) ;; bitwise operators
            )
    (include-shared "170/170")
    (include-shared "170/aux")))
  (include "170/common.scm")
  (include "170/170.scm")
  )
