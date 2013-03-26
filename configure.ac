
# configure package
AC_PREREQ([2.65])
AC_INIT([XBindJoy], [0.1], [sreynoldshaertle@gmail.com])
AM_INIT_AUTOMAKE([-Wall -Werror foreign subdir-objects])

# configure directories
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([src/xbindjoy.c])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([
        Makefile
])

# Check for capabitieis of various programs
AC_USE_SYSTEM_EXTENSIONS
AC_PROG_CC
AC_PROG_CC_C89
AC_PROG_LIBTOOL

AC_C_RESTRICT
AC_C_INLINE
AC_C_CONST


# Enable maximum warnings
m4_ifdef([AX_CFLAGS_WARN_ALL],
         [AX_CFLAGS_WARN_ALL])


# AX_CHECK_COMPILER_FLAGS maco was renamed to AX_CHECK_COMPILE_FLAG
# for both versions
m4_ifdef([AX_CHECK_COMPILE_FLAG],
         [AC_DEFUN([APPEND_FLAG],
                   [AX_CHECK_COMPILE_FLAG([$1], [CFLAGS="$1 $CFLAGS"])])])

m4_ifdef([AX_CHECK_COMPILER_FLAGS],
         [AC_DEFUN([APPEND_FLAG],
                   [AX_CHECK_COMPILER_FLAGS([$1], [CFLAGS="$1 $CFLAGS"])])])

# we found the flag checking macro, check some flags
m4_ifdef([APPEND_FLAG],
         [APPEND_FLAG([-Wextra])
          APPEND_FLAG([-Wconversion])
          APPEND_FLAG([-Wpointer-arith])
          APPEND_FLAG([-Wfloat-equal])
          APPEND_FLAG([-Wshadow])
          APPEND_FLAG([-Wwrite-strings])
          APPEND_FLAG([-Wc++-compat])])

AC_OUTPUT
