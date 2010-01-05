/*
 * dirent.c
 *
 * Author:  Shuichi Kitaguchi <kit@Mew.org>
 * Created: Jun 17, 2001
 * Revised: 
 *
 * Code:
 */

#include <windows.h>
#include <stdio.h>
#include <mbstring.h>

#include <w32.h>
#include <dirent.h>


/*
 * XXX: these functions may return invalid errno.
 */

DIR *
opendir(const char *filename)
{
    DIR *dir;
    size_t len = strlen(filename);
    char *pt;

    dir = malloc(sizeof(DIR));
    if (dir == NULL) return NULL;

    /* initialize DIR strucure */
    memset(dir, 0, sizeof(DIR));
    dir->hFind = INVALID_HANDLE_VALUE;

    if (strlen(filename) == 0){
	strcpy(dir->dd_filename, "*.*");
    } else {
	while ((pt=_mbschr(dir->dd_filename, '/')) != NULL )
	    *pt = '\\';
	if (filename[len-1] == '\\')
	    _snprintf(dir->dd_filename, sizeof(dir->dd_filename),
		      "%s*.*", filename);
	else
	    _snprintf(dir->dd_filename, sizeof(dir->dd_filename),
		      "%s\\*.*", filename);
    }
    
    return dir;
}

struct dirent *
readdir(DIR *dir)
{
    WIN32_FIND_DATA wfd;

    if (dir->hFind == INVALID_HANDLE_VALUE){
	dir->hFind = FindFirstFile(dir->dd_filename, &wfd);
	if (dir->hFind == INVALID_HANDLE_VALUE) return NULL;
    } else {
	if (!FindNextFile(dir->hFind, &wfd)) return NULL;
    }

    /* store file attributes */
    dir->dir.d_ino++;		/* XXX fake */
    dir->dir.d_namlen = strlen(wfd.cFileName);
    strcpy(dir->dir.d_name, wfd.cFileName);
    
    return &dir->dir;
}

int
closedir(DIR *dir)
{
    if (dir->hFind != INVALID_HANDLE_VALUE)
	FindClose(dir->hFind);
    free(dir);

    return 0;
}

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
 * dirent.c ends here
 */
