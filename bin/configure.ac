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

if test "$GCC" = "yes"; then
  CFLAGS="$CFLAGS -Wall"
fi

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
AC_CONFIG_FILES([mew-pinentry])
AC_OUTPUT
