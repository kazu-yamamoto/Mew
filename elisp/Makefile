##
### Makefile.in for byte-compile
###
### Author:  Mew developing team
### Created: Nov 18, 1994
### Revised: Feb 10, 2023

EMACS = emacs
prefix = /usr/local
elispdir = $(prefix)/share/emacs/site-lisp/mew

################################################################
##
## DO NOT EDIT THE FOLLOWINGS
##

OBJS =	mew-addrbook.elc	mew-attach.elc		mew-auth.elc	\
	mew-blvs.elc		mew-bq.elc		mew-cache.elc	\
	mew-complete.elc	mew-config.elc		mew-const.elc	\
	mew-decode.elc		mew-demo.elc		mew-draft.elc	\
	mew-edit.elc		mew-encode.elc		\
	mew-env.elc		mew-env0.elc	\
	mew-ext.elc		mew-fib.elc		mew-func.elc	\
	mew-gemacs.elc		mew-header.elc		mew-highlight.elc \
	mew-imap.elc		mew-imap2.elc		mew-key.elc	\
	mew-lang-jp.elc		mew-lang-kr.elc		mew-lang-latin.elc \
	mew-local.elc		mew-mark.elc		\
	mew-message.elc		mew-mime.elc		mew-minibuf.elc	\
	mew-mule.elc		mew-mule3.elc	\
	mew-net.elc		mew-nntp.elc		mew-nntp2.elc	\
        mew-oauth2.elc \
	mew-pgp.elc		mew-pick.elc		mew-pop.elc	\
	mew-refile.elc		mew-scan.elc		mew-smime.elc	\
	mew-search.elc		mew-smtp.elc		mew-sort.elc	\
	mew-ssh.elc		mew-ssl.elc		mew-passwd.elc	\
	mew-summary.elc		mew-summary2.elc	mew-summary3.elc \
	mew-summary4.elc	mew-syntax.elc		\
	mew-thread.elc		mew-unix.elc		\
	mew-vars.elc		mew-vars2.elc		mew-vars3.elc	\
	mew-varsx.elc		mew-virtual.elc		\
	mew-win32.elc		mew-exec.elc	\
	mew-darwin.elc		mew.elc

SRCS =	mew-addrbook.el	mew-attach.el	mew-auth.el	\
	mew-blvs.el	mew-bq.el	mew-cache.el	\
	mew-complete.el	mew-config.el	mew-const.el	\
	mew-decode.el	mew-demo.el	mew-draft.el	\
	mew-edit.el	mew-encode.el	\
	mew-env.el	mew-env0.el	\
	mew-ext.el	mew-fib.el	mew-func.el	\
	mew-gemacs.el	mew-header.el	mew-highlight.el \
	mew-imap.el	mew-imap2.el	mew-key.el	\
	mew-lang-jp.el	mew-lang-kr.el	mew-lang-latin.el \
	mew-local.el	mew-mark.el	\
	mew-message.el	mew-mime.el	mew-minibuf.el	\
	mew-mule.el	mew-mule3.el	\
	mew-net.el	mew-nntp.el	mew-nntp2.el	\
        mew-oauth2.el \
	mew-pgp.el	mew-pick.el	mew-pop.el	\
	mew-refile.el	mew-scan.el	mew-smime.el	\
	mew-search.el	mew-smtp.el	mew-sort.el	\
	mew-ssh.el	mew-ssl.el	mew-passwd.el	\
	mew-summary.el	mew-summary2.el	mew-summary3.el	\
	mew-summary4.el	mew-syntax.el	\
	mew-thread.el	mew-unix.el	\
	mew-vars.el	mew-vars2.el	mew-vars3.el	\
	mew-varsx.el	mew-virtual.el	\
	mew-win32.el	mew-exec.el	\
	mew-darwin.el	mew.el

TEMPFILE  = temp.el
TEMPFILE2 = temp2.el
DETECT = emacs

CP = cp
RM = rm -f
MKDIR = mkdir -p
# To save Emacses on Windows
PWD =

CNF = config.h config.status config.cache config.log configure.lineno
MK  = Makefile

DESTDIR =

################################################################

all: $(OBJS)

install: install-elisp

################################################################

$(OBJS): el
el: $(TEMPFILE) mew.el
	@echo 'Compiling EL files of Mew ... '
	@echo 'PLEASE IGNORE WARNINGS IF DISPLAYED. TAKE IT EASY!'
	$(EMACS) -batch -q -no-site-file -l ./$(TEMPFILE) -f mew-compile
	@echo 'Compiling EL files of Mew ... done'

$(TEMPFILE):
	@echo '(setq mew-compiling t)' > $(TEMPFILE)
	@echo '(setq load-path (cons "." load-path))' >> $(TEMPFILE)
	@echo '(defun mew-compile () (mapcar (lambda (x) (byte-compile-file x)) (list ' >> $(TEMPFILE)
	@echo $(OBJS) | sed -e 's/\(mew[^ ]*\.el\)c/"\1"/g'  >> $(TEMPFILE)
	@echo ')))' >> $(TEMPFILE)

install-elisp: $(OBJS)
	-@if [ ! -d $(DESTDIR)$(elispdir) ]; then \
		$(MKDIR) $(DESTDIR)$(elispdir); \
	fi
	$(CP) $(SRCS) $(DESTDIR)$(elispdir)
	$(CP) $(OBJS) $(DESTDIR)$(elispdir)

################################################################

detect: $(TEMPFILE2) mew.el
	@echo 'Compiling EL files of Mew ... '
	$(DETECT) -batch -q -no-site-file -l ./$(TEMPFILE2) -f mew-compile
	@echo 'Compiling EL files of Mew ... done'

$(TEMPFILE2):
	@echo '(setq mew-compiling t)' > $(TEMPFILE2)
	@echo '(setq load-path (cons "." load-path))' >> $(TEMPFILE2)
	@echo '(setq hack-local-variables-hook (lambda () (setq lexical-binding t)))' >> $(TEMPFILE2)
	@echo "(defun mew-compile () (mapcar (lambda (x) (byte-compile-file (symbol-name x))) '($(SRCS))))" >> $(TEMPFILE2)

################################################################

clean:
	$(RM) *.elc $(TEMPFILE) $(TEMPFILE2)

distclean:
	$(RM) *.elc $(TEMPFILE) $(TEMPFILE2)

##
## End of Makefile
##
