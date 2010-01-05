/*
 * mew.h
 *
 * Author:  Kazu Yamamoto <Kazu@Mew.org>
 * Created: Jul  7, 2000
 * Revised: Jul 10, 2001
 *
 * Code:
 */

#ifndef _MEW_H_
#define _MEW_H_

#include "config.h"
#include <stdio.h>

/*
 * Common symbol
 */

#define TRUE  1
#define FALSE 0

#define YES 1
#define NO  0

/*
 * File descriptor
 */

#define READ  0
#define WRITE 1
#define SYNC  3

/*
 * File mode: "b" is ignored on UNIX.
 */

#define FDREAD	"rb"
#define FDWRITE	"wb"

/*
 * Control characters
 */

#define NUL  '\0'
#define CR   '\r'
#define LF   '\n'
#define EQ   '='
#define TAB  '\t'
#define SP   ' '
#define DOT  '.'
#define DEL 127

/*
 * .mew-mtime filename
 */

#define MEW_MTIME_FILE   ".mew-mtime"
#define MEW_MTIME_PHRASE "touched by mewl."

/*
 * Suffix
 */

#define SUFFIX   ".mew"

/*
 *
 */

#define FILESEP '/'

#define STRCMP(str1, str2) strncmp(str1, str2, sizeof(str2) - 1)
#define STRCPY(dst, src) do {strncpy(dst, src, sizeof(dst) - 1); dst[sizeof(dst) -1] = NUL;} while (0);
#define STRDUP(dst, src) if (((dst) = strdup((src))) == NULL) warn_exit("memory fault.")
#define MALLOC(p, siz) if (((p) = malloc((siz))) == NULL) warn_exit("memory fault.")
#define REALLOC(p, siz) if (((p) = realloc((p), (siz))) == NULL) warn_exit("memory fault.")

#ifndef HAVE_STRNCASECMP
# ifdef HAVE_STRNICMP
#  define strncasecmp strnicmp
# endif
#endif

#ifndef HAVE_CHDIR
# ifdef HAVE__CHDIR2
#  define chdir _chdir2
# endif
#endif

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
# ifdef HAVE_IO_H
#  include <io.h>
# endif
# ifdef O_BINARY
#  ifndef HAVE_SETMODE
#   ifdef HAVE__SETMODE
#    define setmode _setmode
#   endif
#  endif
#  ifndef HAVE_FILENO
#   ifdef HAVE__FILENO
#    define fileno _fileno
#   endif
#  endif
# define SET_BINARY_MODE(fd) setmode(fileno((fd)),  O_BINARY)
# else /* O_BINARY */
# define SET_BINARY_MODE(fd) {}
# endif /* O_BINARY */
#endif /* HAVE_FCNTL_H */

#ifndef HAVE_USLEEP
# ifdef HAVE_SLEEP
#  define usleep(x) Sleep(x / 1000)
# elif defined(HAVE_POLL)
#  define usleep(x) poll(0, 0, (x)/1000)
# endif
#endif

#ifdef HAVE_VFORK
# define FORK()	vfork()
# define HAVE_FORK 1
#elif defined(HAVE_FORK)
# define FORK()	fork()
#endif

/*
 * External functions and variables
 */

#define public  extern
#define private static

public char *getprognm(char *);
public void warning(const char *, ...);
public void warn_exit(const char *, ...);
public int search_string(char *, char *, int);
public void pattern_init(char *);
public int pattern_match(void);
public char *Getline(FILE *);
public int Getopt(int, char **, const char *);

extern char *warn_prog;
extern int Optind;
extern char *Optarg;

#endif /* _MEW_H_ */

/* 
 * Copyright (C) 2000-2003 Mew developing team.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the team nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * mew.h ends here
 */
