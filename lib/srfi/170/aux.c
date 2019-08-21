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
#include <termios.h>
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

// 3.2  I/O

// started from sexp.c sexp_open_input_file_descriptor

// ~~~~ look up the code that creates the standard ports from stdio, stdout, and stderr

sexp sexp_file_descriptor_to_port (sexp ctx, sexp self, sexp_sint_t n, sexp boxed_fd, sexp is_input, sexp is_binary) {
  sexp_gc_var3(res, str, fileno);

  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, boxed_fd);
  sexp_assert_type(ctx, sexp_booleanp, SEXP_BOOLEAN, is_input);
  sexp_assert_type(ctx, sexp_booleanp, SEXP_BOOLEAN, is_binary);

  sexp_gc_preserve3(ctx, res, str, fileno);
  str = sexp_make_string(ctx, sexp_make_fixnum(SEXP_PORT_BUFFER_SIZE), SEXP_VOID);
  res = sexp_open_input_string(ctx, str);
  fileno = sexp_make_fileno_op (ctx, self, n, boxed_fd, SEXP_TRUE); // ~~~~ last is no_closep
  if (!sexp_exceptionp(res)) {
    sexp_port_fd(res) = fileno;
    sexp_port_offset(res) = sexp_truep(is_input) ? SEXP_PORT_BUFFER_SIZE : 0;
    sexp_pointer_tag(res) = sexp_truep(is_input) ? SEXP_IPORT : SEXP_OPORT;
    sexp_port_binaryp(res) = sexp_truep(is_binary) ? 1 : 0;
    //~~~~     sexp_port_shutdownp(res) = sexp_truep(shutdownp);
    sexp_fileno_count(fileno)++;
  }
  sexp_gc_release3(ctx);
  return res;
}

sexp sexp_fileno_to_fd (sexp ctx, sexp self, sexp_sint_t n, sexp the_fileno) {
  sexp res;
  if (! (sexp_filenop(the_fileno)))
    return sexp_type_exception(ctx, self, SEXP_FILENO, the_fileno);
  res = sexp_make_integer(ctx, sexp_fileno_fd(the_fileno));
  return res;
}


// 3.3  File system

sexp sexp_wrap_utimensat (sexp ctx, sexp self, sexp_sint_t n, sexp the_fd, sexp the_path, sexp the_atime, sexp the_mtime, sexp the_flag) {

  struct timespec times[2];

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
  return SEXP_FALSE;
  } else {
  return SEXP_TRUE;
  }
}


// 3.12  Terminal device control

sexp sexp_25_tcsetattr_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0, sexp arg1, sexp arg2) {
  int err = 0;
  sexp res;
  if (! (sexp_portp(arg0) || sexp_filenop(arg0) || sexp_fixnump(arg0)))
    return sexp_xtype_exception(ctx, self, "not a port or file descriptor",arg0);
  if (! sexp_exact_integerp(arg1))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg1);
  if (! (sexp_pointerp(arg2) && (sexp_pointer_tag(arg2) == sexp_unbox_fixnum(sexp_opcode_arg3_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg3_type(self)), arg2);
  err = tcsetattr((sexp_portp(arg0) ? sexp_port_fileno(arg0) : sexp_filenop(arg0) ? sexp_fileno_fd(arg0) : sexp_unbox_fixnum(arg0)), sexp_sint_value(arg1), (struct termios*)sexp_cpointer_value(arg2));
  if (err) {
  res = SEXP_FALSE;
  } else {
  res = SEXP_TRUE;
  }
  return res;
}

sexp sexp_25_tcgetattr_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0) {
  int err = 0;
  struct termios* tmp1;
  sexp res;
  sexp_gc_var1(res1);
  if (! (sexp_portp(arg0) || sexp_filenop(arg0) || sexp_fixnump(arg0)))
    return sexp_xtype_exception(ctx, self, "not a port or file descriptor",arg0);
  sexp_gc_preserve1(ctx, res1);
  tmp1 = (struct termios*) calloc(1, 1 + sizeof(tmp1[0]));
  err = tcgetattr((sexp_portp(arg0) ? sexp_port_fileno(arg0) : sexp_filenop(arg0) ? sexp_fileno_fd(arg0) : sexp_unbox_fixnum(arg0)), tmp1);
  if (err) {
  res = SEXP_FALSE;
  } else {
  res1 = sexp_make_cpointer(ctx, sexp_unbox_fixnum(sexp_opcode_arg2_type(self)), tmp1, SEXP_FALSE, 1);
  res = res1;
  }
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_make_term_attrs_stub (sexp ctx, sexp self, sexp_sint_t n) {
  struct termios* r;
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), sexp_unbox_fixnum(sexp_opcode_return_type(self)));
  sexp_cpointer_value(res) = calloc(1, sizeof(struct termios));
  r = (struct termios*) sexp_cpointer_value(res);
  memset(r, 0, sizeof(struct termios));
  sexp_freep(res) = 1;
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_termios_get_c_iflag (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  return sexp_make_unsigned_integer(ctx, ((struct termios*)sexp_cpointer_value(x))->c_iflag);
}

sexp sexp_termios_set_c_iflag (sexp ctx, sexp self, sexp_sint_t n, sexp x, sexp v) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  if (! sexp_exact_integerp(v))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, v);
  ((struct termios*)sexp_cpointer_value(x))->c_iflag = sexp_uint_value(v);
  return SEXP_VOID;
}

sexp sexp_termios_get_c_oflag (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  return sexp_make_unsigned_integer(ctx, ((struct termios*)sexp_cpointer_value(x))->c_oflag);
}

sexp sexp_termios_set_c_oflag (sexp ctx, sexp self, sexp_sint_t n, sexp x, sexp v) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  if (! sexp_exact_integerp(v))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, v);
  ((struct termios*)sexp_cpointer_value(x))->c_oflag = sexp_uint_value(v);
  return SEXP_VOID;
}

sexp sexp_termios_get_c_cflag (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  return sexp_make_unsigned_integer(ctx, ((struct termios*)sexp_cpointer_value(x))->c_cflag);
}

sexp sexp_termios_set_c_cflag (sexp ctx, sexp self, sexp_sint_t n, sexp x, sexp v) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  if (! sexp_exact_integerp(v))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, v);
  ((struct termios*)sexp_cpointer_value(x))->c_cflag = sexp_uint_value(v);
  return SEXP_VOID;
}

sexp sexp_termios_get_c_lflag (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  return sexp_make_unsigned_integer(ctx, ((struct termios*)sexp_cpointer_value(x))->c_lflag);
}

sexp sexp_termios_set_c_lflag (sexp ctx, sexp self, sexp_sint_t n, sexp x, sexp v) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  if (! sexp_exact_integerp(v))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, v);
  ((struct termios*)sexp_cpointer_value(x))->c_lflag = sexp_uint_value(v);
  return SEXP_VOID;
}

// x is the termios struct pointer, i is the array index for the value for the c_cc array element to return

sexp sexp_termios_get_c_cc_element (sexp ctx, sexp self, sexp_sint_t n, sexp x, sexp i) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
// make sure i is a fixnum
// printf ("\n\ncc_c element 1 = %u, i = %ld\n\n", ((struct termios*)sexp_cpointer_value(x))->c_cc[1], sexp_unbox_fixnum(i));
  return sexp_make_unsigned_integer(ctx, ((struct termios*)sexp_cpointer_value(x))->c_cc[sexp_unbox_fixnum(i)]);
}

sexp sexp_termios_set_c_cc_element (sexp ctx, sexp self, sexp_sint_t n, sexp x, sexp v, sexp i) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  if (! sexp_exact_integerp(v))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, v);
  ((struct termios*)sexp_cpointer_value(x))->c_lflag = sexp_uint_value(v);
  return SEXP_VOID;
}


sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, const sexp_abi_identifier_t abi) {
  sexp sexp_termios_type_obj;
  sexp_gc_var3(name, tmp, op);

  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return SEXP_ABI_ERROR;

  sexp_define_foreign(ctx, env, "errno", 0, sexp_errno);
  sexp_define_foreign(ctx, env, "set-errno", 1, sexp_set_errno);
  sexp_define_foreign_opt(ctx, env, "integer->error-string", 1, sexp_error_string, SEXP_FALSE); // ~~~~ what the bleep is the false, and why _opt?

  sexp_define_foreign(ctx, env, "%file_descriptor_to_port", 3, sexp_file_descriptor_to_port);
  sexp_define_foreign(ctx, env, "%fileno-to-fd", 1, sexp_fileno_to_fd);

  sexp_define_foreign(ctx, env, "%utimensat", 5, sexp_wrap_utimensat);

  // ~~~~ examine sexp_register_simple_type to create double timespect struct???

  sexp_gc_preserve3(ctx, name, tmp, op);

  name = sexp_intern(ctx, "NCCS", 4);
  sexp_env_define(ctx, env, name, tmp=sexp_make_integer(ctx, NCCS));
  name = sexp_intern(ctx, "VREPRINT", 8);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, VREPRINT));
  name = sexp_intern(ctx, "VLNEXT", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, VLNEXT));
  name = sexp_intern(ctx, "VSTOP", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, VSTOP));
  name = sexp_intern(ctx, "VSTART", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, VSTART));
  name = sexp_intern(ctx, "VSUSP", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, VSUSP));
  name = sexp_intern(ctx, "VQUIT", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, VQUIT));
  name = sexp_intern(ctx, "VKILL", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, VKILL));
  name = sexp_intern(ctx, "VINTR", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, VINTR));
  name = sexp_intern(ctx, "VWERASE", 7);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, VWERASE));
  name = sexp_intern(ctx, "VERASE", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, VERASE));
  name = sexp_intern(ctx, "VEOL2", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, VEOL2));
  name = sexp_intern(ctx, "VEOL", 4);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, VEOL));
  name = sexp_intern(ctx, "VEOF", 4);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, VEOF));
  name = sexp_intern(ctx, "NOFLSH", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, NOFLSH));
  name = sexp_intern(ctx, "PENDIN", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, PENDIN));
  name = sexp_intern(ctx, "FLUSHO", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, FLUSHO));
  name = sexp_intern(ctx, "TOSTOP", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, TOSTOP));
  name = sexp_intern(ctx, "EXTPROC", 7);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, EXTPROC));
  name = sexp_intern(ctx, "IEXTEN", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, IEXTEN));
  name = sexp_intern(ctx, "ICANON", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, ICANON));
  name = sexp_intern(ctx, "ISIG", 4);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, ISIG));
  name = sexp_intern(ctx, "ECHOCTL", 7);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, ECHOCTL));
  name = sexp_intern(ctx, "ECHOPRT", 7);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, ECHOPRT));
  name = sexp_intern(ctx, "ECHONL", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, ECHONL));
  name = sexp_intern(ctx, "ECHO", 4);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, ECHO));
  name = sexp_intern(ctx, "ECHOE", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, ECHOE));
  name = sexp_intern(ctx, "ECHOKE", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, ECHOKE));
  name = sexp_intern(ctx, "CRTSCTS", 7);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, CRTSCTS));
  name = sexp_intern(ctx, "CLOCAL", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, CLOCAL));
  name = sexp_intern(ctx, "HUPCL", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, HUPCL));
  name = sexp_intern(ctx, "PARODD", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, PARODD));
  name = sexp_intern(ctx, "PARENB", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, PARENB));
  name = sexp_intern(ctx, "CREAD", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, CREAD));
  name = sexp_intern(ctx, "CSTOPB", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, CSTOPB));
  name = sexp_intern(ctx, "CS8", 3);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, CS8));
  name = sexp_intern(ctx, "CS7", 3);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, CS7));
  name = sexp_intern(ctx, "CS6", 3);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, CS6));
  name = sexp_intern(ctx, "CS5", 3);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, CS5));
  name = sexp_intern(ctx, "CSIZE", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, CSIZE));
  name = sexp_intern(ctx, "ONLRET", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, ONLRET));
  name = sexp_intern(ctx, "ONOCR", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, ONOCR));
  name = sexp_intern(ctx, "OLCUC", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, OLCUC));
  name = sexp_intern(ctx, "OCRNL", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, OCRNL));
  name = sexp_intern(ctx, "ONLCR", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, ONLCR));
  name = sexp_intern(ctx, "OPOST", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, OPOST));
  name = sexp_intern(ctx, "IUCLC", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, IUCLC));
  name = sexp_intern(ctx, "IMAXBEL", 7);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, IMAXBEL));
  name = sexp_intern(ctx, "IXANY", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, IXANY));
  name = sexp_intern(ctx, "IXOFF", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, IXOFF));
  name = sexp_intern(ctx, "IXON", 4);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, IXON));
  name = sexp_intern(ctx, "ICRNL", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, ICRNL));
  name = sexp_intern(ctx, "IGNCR", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, IGNCR));
  name = sexp_intern(ctx, "INLCR", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, INLCR));
  name = sexp_intern(ctx, "ISTRIP", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, ISTRIP));
  name = sexp_intern(ctx, "INPCK", 5);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, INPCK));
  name = sexp_intern(ctx, "PARMRK", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, PARMRK));
  name = sexp_intern(ctx, "IGNPAR", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, IGNPAR));
  name = sexp_intern(ctx, "BRKINT", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, BRKINT));
  name = sexp_intern(ctx, "IGNBRK", 6);
  sexp_env_define(ctx, env, name, tmp=sexp_make_unsigned_integer(ctx, IGNBRK));
  name = sexp_intern(ctx, "TCSAFLUSH", 9);
  sexp_env_define(ctx, env, name, tmp=sexp_make_integer(ctx, TCSAFLUSH));
  name = sexp_intern(ctx, "TCSADRAIN", 9);
  sexp_env_define(ctx, env, name, tmp=sexp_make_integer(ctx, TCSADRAIN));
  name = sexp_intern(ctx, "TCSANOW", 7);
  sexp_env_define(ctx, env, name, tmp=sexp_make_integer(ctx, TCSANOW));

  name = sexp_c_string(ctx, "termios", -1);
  sexp_termios_type_obj = sexp_register_c_type(ctx, name, sexp_finalize_c_type);
  tmp = sexp_string_to_symbol(ctx, name);
  sexp_env_define(ctx, env, tmp, sexp_termios_type_obj);

  sexp_type_slots(sexp_termios_type_obj) = SEXP_NULL;
  sexp_push(ctx, sexp_type_slots(sexp_termios_type_obj), sexp_intern(ctx, "c_lflag", -1));
  sexp_push(ctx, sexp_type_slots(sexp_termios_type_obj), sexp_intern(ctx, "c_cflag", -1));
  sexp_push(ctx, sexp_type_slots(sexp_termios_type_obj), sexp_intern(ctx, "c_oflag", -1));
  sexp_push(ctx, sexp_type_slots(sexp_termios_type_obj), sexp_intern(ctx, "c_iflag", -1));
  sexp_type_getters(sexp_termios_type_obj) = sexp_make_vector(ctx, SEXP_FOUR, SEXP_FALSE);
  sexp_type_setters(sexp_termios_type_obj) = sexp_make_vector(ctx, SEXP_FOUR, SEXP_FALSE);

  tmp = sexp_make_type_predicate(ctx, name, sexp_termios_type_obj);
  name = sexp_intern(ctx, "term-attrs?", 11);
  sexp_env_define(ctx, env, name, tmp);

  op = sexp_define_foreign(ctx, env, "term-attrs-c_cc-element-set!", 2, sexp_termios_set_c_cc_element);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
  if (sexp_vectorp(sexp_type_setters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_setters(sexp_termios_type_obj), SEXP_THREE, op);

  op = sexp_define_foreign(ctx, env, "term-attrs-c_cc-element", 2, sexp_termios_get_c_cc_element);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
  if (sexp_vectorp(sexp_type_getters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_getters(sexp_termios_type_obj), SEXP_THREE, op);

  op = sexp_define_foreign(ctx, env, "term-attrs-lflag-set!", 2, sexp_termios_set_c_lflag);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
  if (sexp_vectorp(sexp_type_setters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_setters(sexp_termios_type_obj), SEXP_THREE, op);
  op = sexp_define_foreign(ctx, env, "term-attrs-lflag", 1, sexp_termios_get_c_lflag);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
  }
  if (sexp_vectorp(sexp_type_getters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_getters(sexp_termios_type_obj), SEXP_THREE, op);

  op = sexp_define_foreign(ctx, env, "term-attrs-cflag-set!", 2, sexp_termios_set_c_cflag);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
  if (sexp_vectorp(sexp_type_setters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_setters(sexp_termios_type_obj), SEXP_TWO, op);
  op = sexp_define_foreign(ctx, env, "term-attrs-cflag", 1, sexp_termios_get_c_cflag);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
  }
  if (sexp_vectorp(sexp_type_getters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_getters(sexp_termios_type_obj), SEXP_TWO, op);

  op = sexp_define_foreign(ctx, env, "term-attrs-oflag-set!", 2, sexp_termios_set_c_oflag);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
  if (sexp_vectorp(sexp_type_setters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_setters(sexp_termios_type_obj), SEXP_ONE, op);
  op = sexp_define_foreign(ctx, env, "term-attrs-oflag", 1, sexp_termios_get_c_oflag);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
  }
  if (sexp_vectorp(sexp_type_getters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_getters(sexp_termios_type_obj), SEXP_ONE, op);

  op = sexp_define_foreign(ctx, env, "term-attrs-iflag-set!", 2, sexp_termios_set_c_iflag);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
  if (sexp_vectorp(sexp_type_setters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_setters(sexp_termios_type_obj), SEXP_ZERO, op);
  op = sexp_define_foreign(ctx, env, "term-attrs-iflag", 1, sexp_termios_get_c_iflag);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
  }
  if (sexp_vectorp(sexp_type_getters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_getters(sexp_termios_type_obj), SEXP_ZERO, op);

  op = sexp_define_foreign(ctx, env, "make-term-attrs", 0, sexp_make_term_attrs_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
  }

  op = sexp_define_foreign(ctx, env, "%tcsetattr", 3, sexp_25_tcsetattr_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_OBJECT);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(SEXP_OBJECT);
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg3_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
  }

  op = sexp_define_foreign(ctx, env, "%tcgetattr", 1, sexp_25_tcgetattr_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_OBJECT);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(SEXP_OBJECT);
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
  }

  sexp_gc_release3(ctx);

  return SEXP_VOID;
}
