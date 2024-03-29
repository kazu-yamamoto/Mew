AC_INIT
AC_CONFIG_SRCDIR([elisp/mew.el])
AC_CONFIG_SUBDIRS([elisp bin])
AC_ARG_WITH(emacs,
    [  --with-emacs=EMACS      use EMACS],
    emacsbin="${withval}")
AC_SUBST(emacsbin)
AC_ARG_WITH(elispdir,
    [  --with-elispdir=DIR     where elisp files should go],
    elispdir="${withval}")
AC_SUBST(elispdir)
AC_ARG_WITH(etcdir,
    [  --with-etcdir=DIR       where etc files should go],
    etcdir="${withval}")
AC_SUBST(etcdir)

if test -z "${emacsbin}"; then
    AC_PATH_PROGS(emacsbin, emacs, no)
    if test "${emacsbin}" = no; then
	AC_MSG_ERROR([Emacs is not found])
    fi
fi
chk_prefix=
  if test "${datadir}" = "\${prefix}/share" && \
     test -z "${elispdir}" -o -z "${etcdir}"; then
     chk_prefix=yes
  fi

if test ! -x "${emacsbin}"; then
    chk_prefix=
fi
if test "${chk_prefix}"; then
    AC_MSG_CHECKING(for prefix of ${emacsbin})
    AC_CACHE_VAL(emacsbin_cv_emacs_prefix,[
	OUTPUT=./conftest-$$
	echo ${emacsbin}' -q -no-site-file -batch -eval '\''(write-region (princ (expand-file-name ".." invocation-directory)) nil "'${OUTPUT}'" nil 5)'\' >& AS_MESSAGE_LOG_FD 2>&1
	eval ${emacsbin}' -q -no-site-file -batch -eval '\''(write-region (princ (expand-file-name ".." invocation-directory)) nil "'${OUTPUT}'" nil 5)'\' >& AS_MESSAGE_LOG_FD 2>&1
	retval="`cat ${OUTPUT}`"
	rm -f ${OUTPUT}
	emacsbin_cv_emacs_prefix="${retval}"])
    emacs_prefix=${emacsbin_cv_emacs_prefix}
    AC_MSG_RESULT(${emacs_prefix})
fi

if test "${chk_prefix}" -a -z "${elispdir}"; then
    AC_MSG_CHECKING([where lisp files should go])
    if test -d "${emacs_prefix}/share/emacs/site-lisp"; then
	elispdir="${emacs_prefix}/share/emacs/site-lisp/mew"
    fi
    if test "${elispdir}"; then
	AC_MSG_RESULT(${elispdir})
    else
	AC_MSG_RESULT([none (fall back to the default value)])
    fi
fi
if test -z "${elispdir}"; then
    elispdir="\${datadir}/emacs/site-lisp/mew"
fi

if test "${chk_prefix}" -a -z "${etcdir}"; then
    AC_MSG_CHECKING([where etc files should go])
    if test -d "${emacs_prefix}/share/emacs/site-lisp"; then
        etcdir="${emacs_prefix}/share/emacs/site-lisp/mew/etc"
    fi
    if test "${etcdir}"; then
	AC_MSG_RESULT(${etcdir})
    else
	AC_MSG_RESULT([none (fall back to the default value)])
    fi
fi
if test -z "${etcdir}"; then
   etcdir="\${datadir}/emacs/site-lisp/mew/etc"
fi

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

echo_value () {
    case "$1" in
    *\$*)
	eval "echo_value \"$1\""
	;;
    *)
	echo "$1"
	;;
    esac
}

echo
echo "***************************"
echo " Mew configuration results"
echo "***************************"
echo
echo $ECHO_N "use emacs             : $ECHO_C"; echo_value "${emacsbin}"
echo $ECHO_N "elisp files should go : $ECHO_C"; echo_value "${elispdir}"
echo $ECHO_N "etc files should go   : $ECHO_C"; echo_value "${etcdir}"
echo $ECHO_N "exec files should go  : $ECHO_C"; echo_value "${bindir}"
echo $ECHO_N "info files should go  : $ECHO_C"; echo_value "${infodir}"
echo $ECHO_N "man files should go   : $ECHO_C"; echo_value "${mandir}/man1"
echo
echo "Then type 'make' and 'make install'."
echo "For Japanese environment, also 'make install-jinfo'."
echo
