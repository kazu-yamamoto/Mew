AC_INIT
AC_CONFIG_SRCDIR([mewl.c])

AC_PROG_CC
AC_PROG_INSTALL

AC_C_CONST
AC_TYPE_UID_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_CHECK_TYPE(useconds_t, int)
AC_CHECK_TYPE(ssize_t, int)

AC_EXEEXT
AC_OBJEXT

AC_CONFIG_HEADERS([config.h])
AC_HEADER_DIRENT
AC_CHECK_HEADERS(dir.h unistd.h pwd.h fcntl.h io.h sys/time.h sys/file.h)
AC_CHECK_LIB(z, main)
AC_CHECK_FUNCS(strncasecmp strnicmp memmove fgetln fork getpwuid getlogin usleep Sleep setmode _setmode chdir _chdir2 chmod poll futimes fchmod flock lockf snprintf utimes)
AH_TEMPLATE([HAVE_SIGALRM],[Define to 1 if SIGALRM is defined])
AC_MSG_CHECKING(if SIGALRM is defined)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <signal.h>]], [[int i = SIGALRM;]])],[mew_cv_sigalrm=yes],[mew_cv_sigalrm=no])
AC_MSG_RESULT($mew_cv_sigalrm)
if test x$mew_cv_sigalrm = xyes; then
  AC_DEFINE(HAVE_SIGALRM)
fi
AH_TEMPLATE([HAVE_SIGHUP],[Define to 1 if SIGHUP is defined])
AC_MSG_CHECKING(if SIGHUP is defined)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <signal.h>]], [[int i = SIGHUP;]])],[mew_cv_sighup=yes],[mew_cv_sighup=no])
AC_MSG_RESULT($mew_cv_sighup)
if test x$mew_cv_sighup = xyes; then
  AC_DEFINE(HAVE_SIGHUP)
fi
dnl iconv is in libc on glibc but in libiconv on others.  A link test with
dnl <iconv.h> is used since GNU libiconv renames iconv_open() by a macro.
AH_TEMPLATE([HAVE_ICONV],[Define to 1 if iconv is available])
AC_MSG_CHECKING(for iconv)
ICONV_LIBS=
mew_cv_iconv=no
mew_save_LIBS=$LIBS
for mew_lib in "" -liconv; do
  if test x$mew_cv_iconv = xno; then
    LIBS="$mew_save_LIBS $mew_lib"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <iconv.h>]],
      [[iconv_t cd = iconv_open("UTF-8", "ISO-2022-JP"); iconv_close(cd);]])],
      [mew_cv_iconv=yes; ICONV_LIBS=$mew_lib])
  fi
done
LIBS=$mew_save_LIBS
AC_MSG_RESULT($mew_cv_iconv $ICONV_LIBS)
if test x$mew_cv_iconv = xyes; then
  AC_DEFINE(HAVE_ICONV)
fi
AC_SUBST(ICONV_LIBS)

dnl utf8proc gives the pick operators Unicode normalization and case
dnl folding.  It is used when found; --disable-utf8proc builds without it.
dnl utf8proc_uint8_t appeared in utf8proc 1.3, so older versions are not
dnl used.
AH_TEMPLATE([HAVE_UTF8PROC],[Define to 1 if utf8proc is available])
AH_TEMPLATE([HAVE_UTF8PROC_FREE],[Define to 1 if utf8proc has utf8proc_free])
AH_TEMPLATE([UTF8PROC_CHAR_FOLD],[Define to 1 if = and != compare by NFKC_Casefold])
AC_ARG_ENABLE([utf8proc],
  [AS_HELP_STRING([--disable-utf8proc],
    [do not use utf8proc even if it is available])],
  [], [enable_utf8proc=yes])
dnl With utf8proc, = and != of mewl pick apply Unicode full case folding
dnl and NFC regardless of this option.  With character folding they also
dnl ignore compatibility differences such as width and default ignorable
dnl characters, that is, they compare by NFKC_Casefold.
AC_ARG_ENABLE([utf8proc-char-fold],
  [AS_HELP_STRING([--enable-utf8proc-char-fold],
    [compare = and != of mewl pick by NFKC_Casefold, ignoring width and default ignorable characters as well [default=no]])],
  [AS_CASE([$enableval], [yes|no], [],
    [AC_MSG_ERROR([bad value $enableval for --enable-utf8proc-char-fold])])],
  [enable_utf8proc_char_fold=no])
UTF8PROC_LIBS=
mew_cv_utf8proc=no
if test "x$enable_utf8proc" != xno; then
  AC_CHECK_HEADER([utf8proc.h],
    [AC_CHECK_TYPE([utf8proc_uint8_t],
      [AC_CHECK_LIB([utf8proc], [utf8proc_map],
        [mew_cv_utf8proc=yes; UTF8PROC_LIBS=-lutf8proc])],
      [], [[#include <utf8proc.h>]])])
fi
AC_MSG_CHECKING(whether to use utf8proc)
if test "x$enable_utf8proc" = xno; then
  AC_MSG_RESULT([no (disabled)])
else
  AC_MSG_RESULT([$mew_cv_utf8proc])
fi
if test "x$mew_cv_utf8proc" = xyes; then
  AC_DEFINE(HAVE_UTF8PROC)
  AC_MSG_CHECKING([whether = and != fold characters])
  if test "x$enable_utf8proc_char_fold" != xno; then
    AC_DEFINE(UTF8PROC_CHAR_FOLD)
    AC_MSG_RESULT([yes (NFKC_Casefold)])
  else
    AC_MSG_RESULT([no (case folding and NFC)])
  fi
  dnl utf8proc_free (utf8proc 2.12 or later) frees with the C library
  dnl that utf8proc itself uses.  The header and the library may be of
  dnl different versions, so both are checked.
  AC_CHECK_DECL([utf8proc_free],
    [AC_CHECK_LIB([utf8proc], [utf8proc_free], [AC_DEFINE(HAVE_UTF8PROC_FREE)])],
    [], [[#include <utf8proc.h>]])
fi
if test "x$mew_cv_utf8proc" != xyes && test "x$enable_utf8proc_char_fold" != xno; then
  AC_MSG_ERROR([--enable-utf8proc-char-fold needs utf8proc, which is not used])
fi
AC_SUBST(UTF8PROC_LIBS)

if test "$GCC" = "yes"; then
  CFLAGS="$CFLAGS -Wall"
fi

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
AC_CONFIG_FILES([mew-pinentry])
AC_OUTPUT
