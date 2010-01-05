/*
 * dirent.h
 *
 * Author:  Shuichi Kitaguchi <kit@Mew.org>
 * Created: Jun 17, 2001
 * Revised: 
 *
 * Code:
 */

#ifndef __DIRENT_H__
#define __DIRENT_H__


/*
 * Structures
 */

#define d_ino d_fileno

struct dirent {
    DWORD         d_fileno;
    BYTE          d_namlen;
    char          d_name[MAX_PATH + 1];
};

typedef struct _DIR {
    struct dirent  dir;
    HANDLE         hFind;
    char           dd_filename[MAX_PATH+1];
} DIR;


/*
 * Functions
 */

DIR *opendir(const char *);
struct dirent *readdir(DIR *);
int closedir(DIR *);


#endif /* __DIRENT_H__ */

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
 * dirent.h ends here
 */
