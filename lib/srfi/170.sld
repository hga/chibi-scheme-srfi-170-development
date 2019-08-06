
(define-library (srfi 170)
  (export

;;; TMP ----------------

   errno set-errno integer->error-string

   %fileno-to-fd
   %file_descriptor_to_port
   %close

   %delete-directory %delete-file

   %stat %lstat stat:atime stat:mtime stat:ctime

   %opendir %readdir %closedir
   DIR? dirent-name make-directory-object directory-object?
   directory-object-get-DIR directory-object-is-open? set-directory-object-is-open directory-object-dot-files?

   utimens/utime_now utimens/utime_omit
   %utimensat

   posix/path-max

   %realpath

   groups/max %getgroups

   passwd:name passwd:uid %getpwuid_r %getpwnam_r

   %gethostname
   %uname

   timespec:seconds timespec:nanoseconds
   clck-id/realtime clck-id/monotonic
   %clock-gettime



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
   dup->fdes
   close-fdes


   ;; 3.3  File system

   create-directory create-fifo create-hard-link create-symlink
   rename-file
   delete-directory
   set-file-mode set-file-owner set-file-group
   set-file-timespecs timespect/now timespec/omit
   truncate-file

   file-info file-info?
   file-info:device file-info:inode file-info:mode file-info:nlinks
   file-info:uid file-info:gid file-info:rdev file-info:size
   file-info:atime file-info:mtime file-info:ctime

   file-info-directory? file-info-fifo?  file-info-regular?
   file-info-socket?  file-info-block-special?
   file-info-character-special?  file-info-symlink?

   directory-files
   open-directory read-directory close-directory

   real-path

   temp-file-prefix
   create-temp-file
   ;; call-with-temporary-filename


   ;; 3.4  Processes

   ;; 3.4.1  Process objects

   ;; 3.4.2  Process waiting

   ;; 3.4.3  Analysing process status codes

   ;; 3.5  Process state COMPLETE

   umask set-umask
   working-directory set-working-directory
   pid parent-pid
   process-group set-process-group
   priority set-priority
   priority/process priority/process-group priority/user
   nice

   user-login-name
   user-uid user-gid
   user-supplementary-gids
   set-uid set-gid
   user-effective-uid user-effective-gid
   set-user-effective-uid set-user-effective-gid
   set-user-real-and-effective-uid set-user-real-and-effective-gid


   ;; 3.6  User and group database access

   user-info user-info?
   user-info:name user-info:uid user-info:gid user-info:home-dir user-info:shell

   ;; ~~~~ partly implemented below in not bsd
   ;; group-info group-info?
   ;; group-info:name user-info group-info:gid user-info group-info:members


   ;; 3.7  [Intentionally omitted]

   ;; 3.8  System parameters

   system-name
   uname uname?
   uname:os-name uname:node-name uname:release-name uname:version uname:machine
   ;; current-timezone
   ;; current-locale


   ;; 3.9  Signal system COMPLETE

   signal/abrt signal/abrt signal/alrm signal/bus signal/chld
   signal/cont signal/fpe signal/hup signal/ill signal/int
   signal/kill signal/pipe signal/quit signal/segv signal/stop
   signal/term signal/tstp signal/ttin signal/ttou signal/usr1
   signal/usr2 signal/prof signal/sys signal/trap signal/urg
   signal/vtalrm signal/xcpu signal/xfsz

   signal-process signal-process-group


   ;; 3.10  Time COMPLETE

   posix-time monotonic-time


   ;; 3.11  [Intentionally omitted]

   ;; 3.12  Terminal device control

   tty?
   tty-file-name
   ;; with-raw-mode without-echo without-interrupt-chars
   ;; open-control-tty
   ;; become-session-leader
   ;; tty-process-group set-tty-process-group
   ;; control-tty-file-name

   )
  
  (cond-expand ((not bsd)
    (export
#|
;;; TMP

     group:name group:gid %getgrgid_r %getgrnam_r
|#

     ;; 3.1  Errors

     errno/multihop errno/nolink
     ;; STREAMS:
     errno/nodata errno/nostr errno/nosr errno/time


     ;; 3.6  User and group database access

     group-info group-info?
     group-info:name group-info:gid ;; group-info:members

     ;; 3.9  Signal system

     signal/poll
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
            ;; (only (srfi 115) regexp-split)
            (srfi 151) ;; bitwise operators
            )
    (include-shared "170/170")
    (include-shared "170/aux")))
  (include "170/common.scm")
  (include "170/170.scm")
  )
