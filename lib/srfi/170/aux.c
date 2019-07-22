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
#endif

sexp sexp_errno (sexp ctx, sexp self, sexp_sint_t n) {
#ifdef PLAN9
  return SEXP_FALSE;
#else
  return sexp_make_fixnum(errno);
#endif
}

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


sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, const sexp_abi_identifier_t abi) {

  sexp_gc_var2(sym, str); // ~~~~ ?? why 2 vs. 3 or whatever

  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return SEXP_ABI_ERROR;

  sexp_define_foreign(ctx, env, "errno", 0, sexp_errno);
  sexp_define_foreign_opt(ctx, env, "integer->error-string", 1, sexp_error_string, SEXP_FALSE);

  // ~~~~ after instead of before above defines?
  sexp_gc_preserve2(ctx, sym, str);
  sexp_env_define(ctx, env, sym, str); // ??
  sexp_gc_release2(ctx); // ~~~~ ?? why 2 vs. 3 or whatever
  return SEXP_VOID;
}