
(define-library (srfi 170)
  (export

;;; TMP

   errno integer->error-string

   %delete-directory %delete-file

   %stat %lstat

   ;; 3.1  Errors

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

   syscall-error?
   syscall-error:errno syscall-error:message
   syscall-error:procedure syscall-error:data
   errno-error
   

   ;; Useful, but not part of SRFI API:

   ;; will not raise exception unless there is an object
   ;; but it can't delete it
   delete-filesystem-object


   ;; 3.2  I/O

   ;; 3.3  File system

   delete-directory

   file-info file-info?
   file-info:device file-info:inode file-info:mode file-info:nlinks
   file-info:uid file-info:gid file-info:rdev file-info:size
   file-info:atime file-info:mtime file-info:ctime


   ;; 3.4  Processes

   ;; 3.4.1  Process objects

   ;; 3.4.2  Process waiting

   ;; 3.4.3  Analysing process status codes

   ;; 3.5  Process state
   umask set-umask

   ;; 3.6  User and group database access

   ;; 3.7  [Intentionally omitted]

   ;; 3.8  System parameters

   ;; 3.9  Signal system

   ;; 3.10  Time
   clock-gettime
   clck-id/realtime clck-id/monotonic
   make-timespec timespec-seconds timespec-nanoseconds
   timespec?

   ;; 3.11  Environment variables

   ;; 3.12  Terminal device control

   )
  
  (cond-expand ((not bsd)
    (export

     ;; 3.1  Errors

     errno/multihop errno/nolink
     ;; STREAMS
     errno/nodata errno/nostr errno/nosr errno/time)))

  (cond-expand ((not windows)
    (export

     ;; 3.3  File system

     file-info:blksize file-info:blocks)))

  (cond-expand
   (chibi
    (import (scheme base)
	    (chibi)
;;	    (only (chibi filesystem) file-exists?) ;; in R7RS-small
	    (only (chibi ast) errno integer->error-string) ;; ~~~~  until aux.c is up to snuff
	    )
    (include-shared "170/170"))) ;; ~~~~ add aux when it's up to snuff
  (include "170/170.scm"))
