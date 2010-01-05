#ifndef __CONFIG_H__
#define __CONFIG_H__

/* Define to empty if the keyword does not work.  */
/* #undef const */

/* Define if you have the <fcntl.h> header file.  */
#define HAVE_FCNTL_H 1

/* Define if you have the <dirent.h> header file.  */
#define HAVE_DIRENT_H 1

/* Define if you have the <dir.h> header file.  */
#ifdef __BORLANDC__
#define HAVE_DIR_H 1
#endif

/* Define if you have the <direct.h> header file.  */
#ifdef _MSC_VER
#define HAVE_DIRECT_H
#endif

/* Define if you have the <ndir.h> header file.  */
/* #undef HAVE_NDIR_H */

/* Define if you have the <sys/dir.h> header file.  */
/* #undef HAVE_SYS_DIR_H */

/* Define if you have the <sys/ndir.h> header file.  */
/* #undef HAVE_SYS_NDIR_H */

/* Define if you have the <unistd.h> header file.  */
/* #define HAVE_UNISTD_H 1 */

/* Define if you have the <getopt.h> header file.  */
/* #define HAVE_PWD_H 1 */

/* Define if you have the <io.h> header file.  */
#define HAVE_IO_H 1

/* Define if you have the strncasecmp function.  */
/* #define HAVE_STRNCASECMP 1 */

/* Define if you have the strnicmp function.  */
#define HAVE_STRNICMP 1

/* Define if you have the fork function.  */
/* #define HAVE_FORK 1 */

/* Define if you have the vfork function.  */
/* #define HAVE_VFORK 1 */

/* Define if you have the getpwuid function.  */
/* #define HAVE_GETPWUID 1 */

/* Define if you have the usleep function.  */
/* #define HAVE_USLEEP 1 */

/* Define if you have the Sleep function.  */
#define HAVE_SLEEP 1

/* Define if you have the memmove function.  */
#define HAVE_MEMMOVE 1

/* Define if you have the poll function.  */
/* #undef HAVE_POLL */

/* Define if you have the setmode function.  */
#define HAVE_SETMODE 1

/* Define if you have the _setmode function.  */
/* #undef HAVE__SETMODE */

/* Define if you have the chdir function.  */
#define HAVE_CHDIR 1

/* Define if you have the _chdir2 function.  */
/* #undef HAVE__CHDIR2 */

/* Define useconds_t as int if you don't have useconds_t */
#define useconds_t int

/* Define if you have the z library (-lz).  */
#define HAVE_LIBZ 1

/* generic windows header */
#include <windows.h>

/* windows specific definitions */
#include <w32.h>

#endif /* !__CONFIG_H__ */
