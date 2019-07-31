/* Parts of this from the following copyright notices, the total has */
/* the same copyright terms, additions and changes Copyright 2019 */
/* Harold Ancell, Harold Ancell assigns the rights to his additions to */
/* Alex Shinn. */

/*  ast.c -- interface to the Abstract Syntax Tree            */
/*  Copyright (c) 2009-2015 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include <chibi/eval.h>

#ifndef PLAN9
#include <stdlib.h>
#include <errno.h>
#include <sys/stat.h>
#endif

// errno takes no arguments and returns the current errno

sexp sexp_errno (sexp ctx, sexp self, sexp_sint_t n) {
#ifdef PLAN9
  return SEXP_FALSE;
#else
  return sexp_make_fixnum(errno);
#endif
}

// set-errno sets errno to its supplied argument

sexp sexp_set_errno (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
#ifdef PLAN9
  return SEXP_FALSE;
#else
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, x);
  errno = sexp_unbox_fixnum(x);

  return SEXP_VOID;
#endif
}

// integer->error-string takes an optional errno, and returns the
// strerror string for it or the current errno

sexp sexp_error_string (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
#ifdef PLAN9
  return SEXP_FALSE;
#else
  int err;
  if (x == SEXP_FALSE) {
    err = errno;
  } else {
    sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, x);
    err = sexp_unbox_fixnum(x);
  }
  return sexp_c_string(ctx, strerror(err), -1);
#endif
}

// 3.3  File system

sexp sexp_wrap_utimensat (sexp ctx, sexp self, sexp_sint_t n, sexp the_fd, sexp the_path, sexp the_atime, sexp the_mtime, sexp the_flag) {

  struct timespec times[2];
  sexp ret;

  if (! sexp_exact_integerp(the_fd))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, the_fd);
  if (! sexp_stringp(the_path))
    return sexp_type_exception(ctx, self, SEXP_STRING, the_fd);

  if (! sexp_pairp(the_atime))
    return sexp_type_exception(ctx, self, SEXP_PAIR, the_atime);
  if (! (sexp_exact_integerp(sexp_car(the_atime)) || sexp_exact_integerp(sexp_cdr(the_atime))))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, the_atime);
  times[0].tv_sec = sexp_sint_value(sexp_car(the_atime));
  times[0].tv_nsec = sexp_sint_value(sexp_cdr(the_atime));

  if (! sexp_pairp(the_mtime))
    return sexp_type_exception(ctx, self, SEXP_PAIR, the_mtime);
  if (! (sexp_exact_integerp(sexp_car(the_mtime)) || sexp_exact_integerp(sexp_cdr(the_mtime))))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, the_mtime);
  times[1].tv_sec = sexp_sint_value(sexp_car(the_mtime));
  times[1].tv_nsec = sexp_sint_value(sexp_cdr(the_mtime));

  if (! sexp_exact_integerp(the_flag))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, the_flag);

  if (utimensat(sexp_sint_value(the_fd), sexp_string_data(the_path), times, sexp_sint_value(the_flag))) {
  ret = SEXP_FALSE;
  } else {
  ret = SEXP_TRUE;
  }
  return ret;
}


sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, const sexp_abi_identifier_t abi) {

  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return SEXP_ABI_ERROR;

  sexp_define_foreign(ctx, env, "errno", 0, sexp_errno);
  sexp_define_foreign(ctx, env, "set-errno", 1, sexp_set_errno);
  sexp_define_foreign_opt(ctx, env, "integer->error-string", 1, sexp_error_string, SEXP_FALSE);
  sexp_define_foreign(ctx, env, "%utimensat", 5, sexp_wrap_utimensat);

  // ~~~~ examine sexp_register_simple_type to create double timespect struct???

  return SEXP_VOID;
}
