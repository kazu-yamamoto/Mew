/*
 * utils.c
 *
 * Author:  Kazu Yamamoto <Kazu@Mew.org>
 * Created: Sep 29, 2000
 *
 * Code:
 */

#include "mew.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#if !HAVE_MEMMOVE
#define memmove(d, s, n) bcopy((s), (d), (n))
#endif

char *warn_prog;

public char *
getprognm(char *filename)
{
	char *p, *q;

	if ((p = strrchr(filename, FILESEP)) != NULL)
		p++;
	else
		p = filename;

	for (q = p; *q; q++) {
		*q = tolower(*q & 0xff);
	}
	return p;
}

public void
warning(const char *fmt, ...)
{
	va_list ap;

	if (warn_prog != NULL)
		fprintf(stderr, "%s: ", warn_prog);
	va_start(ap, fmt);
	if (fmt != NULL)
                vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
}

public void
warn_exit(const char *fmt, ...)
{
	va_list ap;

	if (warn_prog != NULL)
		fprintf(stderr, "%s: ", warn_prog);
	va_start(ap, fmt);
	if (fmt != NULL)
                vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	exit(EXIT_FAILURE);
}

#ifndef HAVE_SNPRINTF
public int
snprintf(char *buf, int len, const char *fmt, ...)
{
	int ret;
	va_list ap;

	va_start(ap, fmt);
	ret = vsprintf(buf, fmt, ap);
	va_end(ap);
	return ret;
}
#endif

#ifndef HAVE_FGETLN
private char*
fgetln(FILE *fp, size_t *len)
{
	static size_t bufsize = 0;
	static char *buf = NULL;
	char tmp[BUFSIZ];
	size_t readlen;

	if (buf == NULL) {
		bufsize = BUFSIZ;
		MALLOC(buf, bufsize);
	}

	*len = 0;
	while (1) {
		if (fgets(tmp, BUFSIZ, fp) == NULL)
			return NULL;
		readlen = strlen(tmp);

		if (*len + readlen > bufsize) {
			bufsize += BUFSIZ;
			REALLOC(buf, bufsize);
		}
		memmove(&buf[*len], tmp, readlen);
		*len += readlen;
		if (readlen < BUFSIZ - 1 || tmp[BUFSIZ - 2] == '\n')
			break;
	}
	return buf;
}
#endif

char*
Getline(FILE* fp)
{
	char *s, *t;
	size_t len;
	if ((t = fgetln(fp, &len)) == NULL)
		return NULL;
	MALLOC(s, len+1);
	memmove(s, t, len);
	s[len] = '\0';
	return s;
}

int  Optind = 1;
char *Optarg = NULL;

public int
Getopt(int argc, char *argv[], const char *fmt)
{
	char *p, *q, c;

	Optarg = NULL;
	if (Optind >= argc)
		return EOF;
	p = argv[Optind];

	if (*p++ != '-')
		return EOF;
	c = *p;
	if (c == NUL)
		return EOF;
	if (*(p + 1) != NUL)
		warn_exit("unknown long option '-%s'.", p);
	if ((q = strchr(fmt, c)) == NULL)
		warn_exit("unknown option '-%c'.", c);
	if (++q != NULL && *q == ':') {
		if (++Optind >= argc)
			warn_exit("no parameter for '-%c'.", c);
		Optarg = argv[Optind];
	}
	Optind++;
	return c;
}

/* 
 * Copyright (C) 2000-2005 Mew developing team.
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
 * utils.c ends here
 */
