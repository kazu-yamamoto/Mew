/*
 * w32.h
 *
 * Author:  Shuichi Kitaguchi <kit@Mew.org>
 * Created: Jun 17, 2001
 * Revised: 
 *
 * Code:
 */


/*
 * Typedefs, definitions and functions for Win32 environment.
 */

#ifndef __W32_H__
#define __W32_H__


/* includes */
#include <direct.h>		/* for chdir()/mkdir()/... */

/* additional types for sys/types.h */

#ifndef _POSIX_SOURCE
typedef unsigned char   u_char;
typedef unsigned short  u_short;
typedef unsigned int    u_int;
typedef unsigned long   u_long;
typedef unsigned short  ushort;         /* Sys V compatibility */
typedef unsigned int    uint;           /* Sys V compatibility */
#endif

typedef unsigned char		u_int8_t;
typedef unsigned short		u_int16_t;
typedef unsigned long		u_int32_t;
typedef unsigned __int64	u_int64_t;
typedef signed char		int8_t;
typedef signed short		int16_t;
typedef signed long		int32_t;
typedef signed __int64		int64_t;

typedef u_int32_t pid_t;	/* process ID */

/* Borland C++ has uid_t and gid_t. Why? */
#ifndef __BORLANDC__
typedef u_int32_t uid_t;	/* user ID */
typedef u_int32_t gid_t;	/* group ID */
#endif

/* definitions */

#ifndef inline
#define inline __inline
#endif

/* POSIX macros */

#include <sys/stat.h>

#ifndef S_IFLNK
#define S_IFLNK 0120000		/* symbolic link */
#endif
#ifndef S_ISUID
#define S_ISUID 0004000		/* set user id on execution */
#endif
#ifndef S_ISGID
#define S_ISGID 0002000		/* set group id on execution */
#endif
#ifndef S_IRWXU
#define S_IRWXU 0000700           /* RWX for owner */
#endif
#ifndef S_IRUSR
#define S_IRUSR 0000400           /* R for owner */
#endif
#ifndef S_IWUSR
#define S_IWUSR 0000200           /* W for owner */
#endif
#ifndef S_IXUSR
#define S_IXUSR 0000100           /* E for owner */
#endif
#ifndef S_IRWXG
#define S_IRWXG 0000070           /* RWX for group */
#endif
#ifndef S_IRGRP
#define S_IRGRP 0000040           /* R for group */
#endif
#ifndef S_IWGRP
#define S_IWGRP 0000020           /* W for group */
#endif
#ifndef S_IXGRP
#define S_IXGRP 0000010           /* X for group */
#endif
#ifndef S_IRWXO
#define S_IRWXO 0000007           /* RWX for other */
#endif
#ifndef S_IROTH
#define S_IROTH 0000004           /* R for other */
#endif
#ifndef S_IWOTH
#define S_IWOTH 0000002           /* W for other */
#endif
#ifndef S_IXOTH
#define S_IXOTH 0000001           /* E for other */
#endif

#ifndef S_ISDIR
#define S_ISDIR(mode) (((mode)&(_S_IFMT))==(_S_IFDIR))
#endif
#ifndef S_ISREG
#define S_ISREG(mode) (((mode)&(_S_IFMT))==(_S_IFREG))
#endif


/* supplements for string.h */

#define strcasecmp     stricmp
#define strncasecmp    strnicmp
#define snprintf       _snprintf
#if _MSC_VER < 1500
#define vsnprintf      _vsnprintf
#endif
#define lstat          stat

size_t strlcpy(char *, const char*, size_t);
size_t strlcat(char *, const char*, size_t);

char *realpath(const char *, char*);

#ifdef INET6
#include <winsock2.h>
#else
#include <winsock.h>
#endif

int  wsa_socketpair(SOCKET *);
void wsa_create_terminal(SOCKET);
char *wsa_strerror(int);
void wsa_perror(char *);
#define wsa_lasterror() WSAGetLastError()


/* On Win32 environment, uid/gid does not exist. */
#define _W32_USER_ID       0
#define _W32_GROUP_ID      0



#endif /* __W32_H__ */

/* 
 * Copyright (C) 2001-2003 Mew developing team.
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
 * w32.h ends here
 */
