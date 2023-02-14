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

if test "$GCC" = "yes"; then
  CFLAGS="$CFLAGS -Wall"
fi

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
AC_CONFIG_FILES([mew-pinentry])
AC_OUTPUT
