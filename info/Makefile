###
### Makefile for info
###
### Author:  Kazu Yamamoto <Kazu@Mew.org>
### Created: Aug 22, 1999
### Revised: 

###
### (1) make EMACS=xemacs
### (2) make install
###

################################################################
##
## EDIT THE FOLLOWINGS
##

prefix=/usr/local
infodir=$(prefix)/info

EMACS=emacs

INSTALLINFO = `which install-info`
#INSTALLINFO = /usr/local/bin/install-info

#INSTALLINFO_OPTIONS = --section Emacs Emacs
#INSTALLINFO_JOPTIONS =
#infodir = 
INSTALLINFO_ARGS  = --info-dir=$(infodir) mew.info
#INSTALLINFO_ARGS  = mew.info 
INSTALLINFO_JARGS = --info-dir=$(infodir) mew.ja.info


RM = rm -f
MKDIR = mkdir -p
TOUCH = touch
# To save Emacses on Windows
PWD =

################################################################
##
## DO NOT EDIT THE FOLLOWINGS
##

DESTDIR =

all: info
info:
	sed -e 's/@setfilename mew.ja.info/@setfilename mew.info/' \
	    -e 's/@set ja/@set en/' mew.texi > tmp.texi
	mv tmp.texi mew.texi
	$(RM) mew.info*
	$(TOUCH) mew.info
	$(EMACS) -batch -q -no-site-file \
		--eval "(set-default-coding-systems 'utf-8-unix)" \
		-l texinfmt -f batch-texinfo-format mew.texi

jinfo:
	sed -e 's/@setfilename mew.info/@setfilename mew.ja.info/' \
	    -e 's/@set en/@set ja/' mew.texi > tmp.texi
	mv tmp.texi mew.texi
	$(RM) mew.ja.info*
	$(TOUCH) mew.ja.info
	$(EMACS) -batch -q -no-site-file \
		--eval "(set-default-coding-systems 'utf-8-unix)" \
		-l texinfmt -f batch-texinfo-format mew.texi

install: install-info
install-info:
	-@if [ ! -d $(DESTDIR)$(infodir) ]; then \
		$(MKDIR) $(DESTDIR)$(infodir); \
	fi; \
	cp mew.info* $(DESTDIR)$(infodir)
	-@if [ -z "$(DESTDIR)" ] && [ -x "$(INSTALLINFO)" ]; then \
		$(INSTALLINFO) $(INSTALLINFO_OPTIONS) $(INSTALLINFO_ARGS); \
	else \
		echo "Don't forget to edit $(infodir)/dir for Mew."; \
	fi

install-jinfo:
	-@if [ ! -d $(DESTDIR)$(infodir) ]; then \
		$(MKDIR) $(DESTDIR)$(infodir); \
	fi; \
	cp mew.ja.info* $(DESTDIR)$(infodir)
	-@if [ -z "$(DESTDIR)" ] && [ -x "$(INSTALLINFO)" ]; then \
		$(INSTALLINFO) $(INSTALLINFO_JOPTIONS) $(INSTALLINFO_JARGS); \
	else \
		echo "Don't forget to edit $(infodir)/dir for Mew."; \
	fi
##
## End of Makefile
##
