;; Unlike the scsh version, will raise an exception if there is a
;; object it can't delete.  If no object, no exception.

(define (delete-filesystem-object fname)
  (if (file-exists? fname)
      (if (file-info-directory? (file-info fname))
          (if (not (delete-directory fname))
              (errno-error (errno) delete-filesystem-object fname))
          (if (not (delete-file fname))
              (errno-error (errno) delete-filesystem-object fname)))))

;;> The fundamental directory iterator.  Applies \var{kons} to
;;> each filename in directory \var{dir} and the result of the
;;> previous application, beginning with \var{knil}.  With
;;> \var{kons} as \scheme{cons} and \var{knil} as \scheme{'()},
;;> equivalent to \scheme{directory-files}.

(define (directory-fold dir kons knil . o)
  (let-optionals o ((dot-files? #f))
    (let ((do (open-directory dir dot-files?)))
      (let lp ((res knil))
        (let ((file (read-directory do)))
          (if (not (eof-object? file))
              (lp (kons file res))
              (begin (close-directory do) res)))))))

(define (replace-ampersands the-string user-name)
  the-string) ;; ~~~~ get the framework in place

(define (parse-gecos gecos user-name)
  (let* ((the-list (regexp-split "," gecos))
         (first-element (car the-list))
         (maybe-first-element (replace-ampersands first-element user-name)))
    (if maybe-first-element
        (set-car! the-list maybe-first-element))
        the-list))
