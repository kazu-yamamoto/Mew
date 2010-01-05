/*
 *  incm - incorporating new mails
 *
 *  Author:  Yasunari Momoi <momo@bug.org>
 *  Created: 2000/10/19
 */

#include "mew.h"

private char version_message[] = "version 5.3 20060727 Yasunari Momoi";

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <pwd.h>
#include <signal.h>
#include <errno.h>

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#if HAVE_DIRENT_H
# include <dirent.h>
#endif

#if HAVE_FCNTL_H
# include <fcntl.h>
#endif

#if HAVE_SYS_FILE_H
# include <sys/file.h>
#endif

#if HAVE_UNISTD_H
# include <unistd.h>
#endif

enum MBOXTYPE {
	T_UNKNOWN,
	T_MAILDIR,
	T_MBOX,
	T_STDIN,
};

enum MBOXSTATE {
	ST_UNKNOWN,
	ST_HEADER,
	ST_BODY_AFTER_EMPTY_LINE,
	ST_BODY,
};

#define FBUFSIZ		(BUFSIZ * 32)
#ifndef PATH_MAX
# define PATH_MAX	1024
#endif

private char	FileBuf[FBUFSIZ];
private char	Mbox[PATH_MAX];
private char	MboxLock[PATH_MAX];
private char	*InboxDir = NULL;
private char	*Suffix = NULL;
private int	Use_Suffix = FALSE;
private int	Suffix_len;
private int	MboxType;
private int	Backup;
private int	GetCur;
private int	UseCL;
private int	PreserveUnixFrom;
private int	CreateMTime = TRUE;
private int	FileMode = S_IRUSR | S_IWUSR;
private int	Exit = 0;

/****************************************************************
 *
 * prototype
 *
 */

private void	error(const char *, ...);
private void	usage(const char *);
private void	help(const char *);
private void	version(const char *);
private void	init_env(int, char **);
private int	get_number(char *);
private int	get_last_seq(void);
private int	compare_string(char **, char **);
private int	copyfile(char *, char *);
private void	movefile(char *, char *, char *, int);
private int	maildir_names(const char *, char **, char **, char **);
private int	new_inbox_file(int, char[]);
private FILE	*open_new_inbox_file(int *, char[]);
private int	get_from_dir(int, char *, char *, int);
private int	process_maildir(int);
private int	lock_mbox(char *);
private void	unlock_mbox(char *);
private int	process_mbox(int);
private int	process_stdin(int);
private void	process(void);
private int	check_mbox_type(const char *);
private void	sanity_check(void);


#if !HAVE_FLOCK
# if HAVE_LOCKF
#  define flock(a, b)	lockf(a, b, 0)
#  define LOCK_EX	F_LOCK
# endif
#endif

#ifndef S_ISDIR
# define S_ISDIR(m)	(((m) & S_IFMT) == S_IFDIR)
#endif
#ifndef S_ISREG
# define S_ISREG(m)	(((m) & S_IFMT) == S_IFREG)
#endif

private char *
Getlogin(void)
{
#ifdef HAVE_GETLOGIN
	{
		char *user;
		if ((user = getlogin()) != NULL)
			return user;
	}
#endif
#ifdef HAVE_GETPWUID
	{
		struct passwd *pw;
		if ((pw = getpwuid(getuid())) == NULL)
			return NULL;
		else
			return pw->pw_name;
	}
#endif
	return NULL;
}

private char *
Gethomedir(void)
{
	char *home;

	if ((home = getenv("HOME")) != NULL)
		return home;
#ifdef HAVE_GETPWUID
	{
		struct passwd *pw;
		if ((pw = getpwuid(getuid())) != NULL)
			return pw->pw_dir;
	}
#endif
	return NULL;
}

private void
error(const char *fmt, ...)
{
	va_list ap;
	if (warn_prog != NULL)
		fprintf(stderr, "%s: ", warn_prog);
	va_start(ap, fmt);
	if (fmt != NULL)
		vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
	if (strlen(MboxLock) > 0)
		unlock_mbox(MboxLock);
	exit(EXIT_FAILURE);
}

/****************************************************************
 * 
 * options and usages
 *
 */


#define LOCK_SUFFIX	".lock"
#define MAILDIR		"Maildir"
#define MAILDIR_NEW	"new"
#define MAILDIR_CUR	"cur"
#define MAILDIR_TMP	"tmp"

private void
usage(const char *progname) {
	fprintf(stderr, "Usage: %s [-abchosv] [-d maildir] [-i inboxdir] [-x suffix]\n", progname);
}

private const char *
help_message[] = {
	"    -h            Display this help message.",
	"    -v            Display the version.",
	"    -d <mail>     Path to mbox/maildir.",
	"    -m <mail>     Path to mbox/maildir.",
	"    -s            Read one mail from stdin instead of mbox/maildir.",
	"    -i <inboxdir> Path to inboxdir.",
	"    -b            Backup mail.",
	"                    mbox: No truncate mbox file.",
	"                    maildir: To maildir/cur directory.",
	"    -a            Retrieve all mail from maildir/{cur,new} directory.",
	"                  (no backup) (for maildir)",
	"    -c            Use Content-Length: field. (for mbox)",
	"    -u            Don't create inboxdir/.mew-mtime file.",
	"    -f            Preserve Unix From (Envelope Sender). (for mbox)",
	"    -p <fmode>    Specify file permission. (for mbox)",
	"    -o            Use the suffix when creating messages.",
	"    -x <suffix>   Use this suffix.",
	NULL
};

private void
help(const char *progname) {
	const char **p = help_message;

	fprintf(stderr, "Help: %s\n\n", progname);
	fprintf(stderr, " Incorporating new mails.\n\n");
	usage(progname);
	while (*p) fprintf(stderr, "%s\n", *p++);
}

private void
version(const char *progname) {
	fprintf(stderr, "%s %s\n", progname, version_message);
}

private int
check_mbox_type(const char *path)
{
	struct stat sb;

	if (stat(path, &sb))
		return T_UNKNOWN;
	if (S_ISDIR(sb.st_mode)) {
		char* newdir;
		char* curdir;

		if (maildir_names(path, &newdir, &curdir, NULL))
			error("maildir name is not set (%s)", path);
		if (stat(newdir, &sb))
			return T_UNKNOWN;
		if (!S_ISDIR(sb.st_mode) || access(newdir, R_OK|W_OK|X_OK))
			return T_UNKNOWN;

		if (Backup) {
			if (stat(curdir, &sb))
				return T_UNKNOWN;
			if (!S_ISDIR(sb.st_mode) || access(curdir, W_OK))
				return T_UNKNOWN;
		}
		return T_MAILDIR;
	}
	else if (S_ISREG(sb.st_mode))
		return T_MBOX;
	else
		return T_UNKNOWN;
}

private const char *
mbox_path_list[] = {
	"/var/mail/",
	"/var/spool/mail/",
	"/usr/spool/mail/",
	NULL
};

private void
search_mbox_path(void)
{
	char *home, *mail, *user;

	if ((home = Gethomedir()) != NULL) {
		if (strlen(home) + 9 > PATH_MAX)
			error("pathname too long (%s)", home);
		snprintf(Mbox, sizeof(Mbox), "%s/%s", home, MAILDIR);
		if (check_mbox_type(Mbox) != T_UNKNOWN)
			return;
	}
	if ((mail = getenv("MAIL")) != NULL) {
		if (strlen(mail) + 1 > PATH_MAX)
			error("pathname too long (%s)", mail);
		STRCPY(Mbox, mail);
		if (check_mbox_type(Mbox) != T_UNKNOWN)
			return;
	}
	if ((user = Getlogin()) != NULL) {
		int i;
		for (i = 0; mbox_path_list[i] != NULL; i++) {
			if (strlen(mbox_path_list[i]) + strlen(user) + 1
			    > PATH_MAX)
				error("pathname too long (%s)", user);
			snprintf(Mbox, sizeof(Mbox), "%s%s", mbox_path_list[i], user);
			if (check_mbox_type(Mbox) != T_UNKNOWN)
				return;
		}
	}
	snprintf(Mbox, sizeof(Mbox), ".");
	return;
}

void
sig_exit(int signo)
{
	Exit = 1;
}

void
sig_ignore(int signo)
{
	/* ignore signal */
}

private void
set_sighandler(void)
{
	if (signal(SIGHUP, sig_ignore) == SIG_ERR)
		error("can't catch SIGHUP\n");
	if (signal(SIGINT, sig_exit) == SIG_ERR)
		error("can't catch SIGINT\n");
	if (signal(SIGALRM, sig_ignore) == SIG_ERR)
		error("can't catch SIGALRM\n");
	if (signal(SIGTERM, sig_ignore) == SIG_ERR)
		error("can't catch SIGTERM\n");
}

private void
init_env(int argc, char **argv)
{
	set_sighandler();
	STRDUP(InboxDir, ".");
	MboxType = T_UNKNOWN;
	Backup = FALSE;
	UseCL = FALSE;
	search_mbox_path();
	STRDUP(Suffix, SUFFIX);
	Suffix_len = strlen(Suffix);
}

private int
get_number(char *s)
{
	int num = 0;
	unsigned char c;

	while ((c = *s) != NUL && isdigit(c)) {
		num = num * 10 + c - '0';
		s++;
	}

	if (num == 0)
		return 0;
	else if (*s == NUL)
		return num;
	else if (strncmp(s, Suffix, Suffix_len) == 0 && s[Suffix_len] == NUL)
		return num;
	else
		return 0;
}

private int
get_last_seq(void)
{
	struct dirent *dp;
	DIR *dirp;
	int last = 0;
	int seq;

	if ((dirp = opendir(InboxDir)) == NULL)
		error("opendir(%s)", InboxDir);
	while ((dp = readdir(dirp)) != NULL) {
		if ((seq = get_number(dp->d_name)) == 0)
			continue;
		last = last > seq ? last : seq;
	}
	closedir(dirp);
	return last;
}

private int
compare_string(char **i, char **j)
{
	return strcmp(*i, *j);
}

private int
copyfile(char *src, char *dst)
{
	struct timeval tv[2];
	struct stat sb;
	int srcfd, dstfd;
	ssize_t rlen, wlen;

	if ((srcfd = open(src, O_RDONLY, 0)) < 0)
		error("open(%s) for read", src);
	if (fstat(srcfd, &sb))
		error("fstat(%s)", src);
	if ((dstfd = open(dst, O_EXCL | O_CREAT | O_WRONLY | O_TRUNC, 0)) < 0)
		error("open(%s) for write", dst);
	while ((rlen = read(srcfd, FileBuf, FBUFSIZ)) > 0) {
		if ((wlen = write(dstfd, FileBuf, rlen)) != rlen) {
			if (close(dstfd))
				goto werr;
			unlink(dst);
			error("write(%s) (read %d bytes/write %d bytes)",
			      dst, rlen, wlen);
		}
	}
	if (rlen < 0) {
		if (close(dstfd))
			goto werr;
		unlink(dst);
		error("read(%s)", src);
	}
	close(srcfd);

	tv[0].tv_sec = sb.st_atime;
	tv[0].tv_usec = 0;
	tv[1].tv_sec = sb.st_mtime;
	tv[1].tv_usec = 0;
#if HAVE_FUTIMES
	if (futimes(dstfd, tv))
		warning("futimes(%s) failed", dst);
#endif
#if HAVE_FCHMOD
	if (fchmod(dstfd, sb.st_mode))
		warning("fchmod(%s) failed", dst);
#endif
	close(dstfd);
#if !HAVE_FUTIMES
	if (utimes(dst, tv))
		warning("utimes(%s) failed", dst);
#endif
#if !HAVE_FCHMOD
	if (chmod(dst, sb.st_mode))
		warning("chmod(%s) failed", dst);
#endif
	return 0;

 werr:
	close(srcfd);
	return -1;
}

private void
movefile(char *fromfile, char *tofile, char *backupfile, int backup)
{
	if (backup && backupfile != NULL) {
		if (copyfile(fromfile, tofile))
			error("copyfile(%s, %s)", fromfile, tofile);
		if (rename(fromfile, backupfile))
			error("rename(%s, %s)", fromfile, backupfile);
	}
	else if (backup) {
		if (copyfile(fromfile, tofile))
			error("copyfile(%s, %s)", fromfile, tofile);
	}
	else {
		if (rename(fromfile, tofile)) {
			if (errno != EXDEV)
				error("rename(%s, %s)", fromfile, tofile);
			if (copyfile(fromfile, tofile))
				error("copyfile(%s, %s)", fromfile, tofile);
			if (unlink(fromfile))
				error("unlink(%s)", fromfile);
		}
	}
}

/* maildir has {new,cur,tmp} subdirectory. */
private int
maildir_names(const char *maildir, char **newdir, char **curdir, char **tmpdir)
{
	int len = strlen(maildir) + strlen(MAILDIR_NEW) + 2;

	if (maildir == NULL || strlen(maildir) <= 0)
		return -1;
	if (newdir != NULL) {
		MALLOC(*newdir, len);
		snprintf(*newdir, len, "%s/%s", maildir, MAILDIR_NEW);
	}
	if (curdir != NULL) {
		MALLOC(*curdir, len);
		snprintf(*curdir, len, "%s/%s", maildir, MAILDIR_CUR);
	}
	if (tmpdir != NULL) {
		MALLOC(*tmpdir, len);
		snprintf(*tmpdir, len, "%s/%s", maildir, MAILDIR_TMP);
	}
	return 0;
}

/* *WARNING* inboxfile requires PATH_MAX bytes */
private int
new_inbox_file(int seq, char inboxfile[])
{
	char num[PATH_MAX];
	char *suffix = (Use_Suffix == YES) ? Suffix : "";

	do {
		snprintf(num, sizeof(num), "%d%s", ++seq, suffix);
		if (strlen(InboxDir) + strlen(num) + 2 > PATH_MAX)
			error("pathname too long (%s/%s)", InboxDir, num);
		snprintf(inboxfile, PATH_MAX, "%s/%s", InboxDir, num);
		if (access(inboxfile, F_OK) && errno == ENOENT)
			break;
	} while (TRUE);
	return seq;
}

/* *WARNING* inboxfile requires PATH_MAX bytes */
private FILE *
open_new_inbox_file(int *seq, char inboxfile[]) 
{
	char num[PATH_MAX];
	int flag = O_EXCL | O_CREAT | O_WRONLY;
	int fd;
	FILE *fp = NULL;
	char *suffix = (Use_Suffix == YES) ? Suffix : "";

	for (;;) {
		snprintf(num, sizeof(num), "%d%s", ++*seq, suffix);
		if (strlen(InboxDir) + strlen(num) + 2 > PATH_MAX)
			error("pathname too long (%s/%s)", InboxDir, num);
		snprintf(inboxfile, PATH_MAX, "%s/%s", InboxDir, num);
		if ((fd = open(inboxfile, flag, FileMode)) >= 0 ||
		    errno != EEXIST)
			break;
		usleep(rand() % 199);
	}
	if (fd < 0)
		warning("open(%s) for write", inboxfile);
	else {
		if ((fp = fdopen(fd, FDWRITE)) == NULL)
			warning("open(%s) for write", inboxfile);
	}
	return fp;
}

private int
get_from_dir(int seq, char *fromdir, char *backupdir, int backup)
{
	struct stat sb;
	struct dirent *dp;
	DIR *dirp;
	char mailfile[PATH_MAX];
	char inboxfile[PATH_MAX];
	char backupfile[PATH_MAX];
	char **list;
	int listsize = BUFSIZ;
	int listend = 0;
	int i;

	MALLOC(list, sizeof(char *)*listsize);
	if ((dirp = opendir(fromdir)) == NULL)
		error("opendir(%s)", fromdir);
	while ((dp = readdir(dirp)) != NULL) {
		if (strlen(fromdir) + strlen(dp->d_name) + 2 > PATH_MAX)
			error("pathname too long (%s/%s)",
			      fromdir, dp->d_name);
		snprintf(mailfile, sizeof(mailfile), "%s/%s", fromdir, dp->d_name);
		if (stat(mailfile, &sb))
			continue;
		if (!(S_ISREG(sb.st_mode) && (sb.st_mode & S_IRUSR)))
			continue;
		if (listend >= listsize) {
			listsize *= 2;
			if ((list = (char **)
			     realloc(list, sizeof(char *)*listsize)) == NULL)
				error("realloc");
		}
		STRDUP(list[listend++], dp->d_name);
	}
	closedir(dirp);

	qsort(list, listend, sizeof(char *),
	      (int (*)(const void *, const void *))compare_string);

	for (i = 0; i < listend; i++) {
		seq = new_inbox_file(seq, inboxfile);
		if (strlen(fromdir) + strlen(list[i]) + 2 > PATH_MAX)
			error("pathname too long (%s/%s)",
			      fromdir, list[i]);
		snprintf(mailfile, sizeof(mailfile), "%s/%s", fromdir, list[i]);
		if (backup && backupdir != NULL) {
			if (strlen(backupdir) + strlen(list[i]) + 6 > PATH_MAX)
				error("pathname too long (%s/%s)",
				      backupdir, list[i]);
			snprintf(backupfile, sizeof(backupfile), "%s/%s:2,S", backupdir, list[i]);
			movefile(mailfile, inboxfile, backupfile, backup);
		}
		else
			movefile(mailfile, inboxfile, NULL, backup);
		printf("%d\n", seq);
	}
	return seq;
}

private int
process_maildir(int seq)
{
	char *newdir, *curdir;
	if (maildir_names(Mbox, &newdir, &curdir, NULL))
		error("maildir name is not set (%s)", Mbox);
	if (GetCur)
		seq = get_from_dir(seq, curdir, NULL, Backup);
	return get_from_dir(seq, newdir, curdir, Backup);
}

private int
lock_mbox(char *lockfile)
{
	int fd;
	int retry = 5;

	while (TRUE) {
	  if ((fd = open(lockfile, O_WRONLY | O_CREAT | O_EXCL, 0666)) < 0) {
			if (errno == EACCES || errno == EROFS)
				return 1; /* doesn't need a lockfile, maybe. */
			else if (errno != EEXIST)
				error("open(%s)", lockfile);
			if (retry-- <= 0)
				error("can't get lock(%s)", lockfile);
		}
		else {
			/* lock succeeded. */
			write(fd, "0", 1);
			close(fd);
			return 0;
		}
		sleep(2);
	}
}

private void
unlock_mbox(char *lockfile)
{
	if (strlen(lockfile) > 0)
		unlink(lockfile);
}

private int
process_mbox(int seq)
{
	char inboxfile[PATH_MAX];
	char emptyline[3];
	char *ln;
	int srcfd, oflag;
	FILE *srcfp = NULL;
	FILE *dstfp = NULL;
	int state = ST_UNKNOWN;
	int bytes = -1;		/* UseCL (Content-Length:) */

	if (strlen(Mbox) + strlen(LOCK_SUFFIX) + 1 > PATH_MAX)
		error("pathname too long (%s%s)", Mbox, LOCK_SUFFIX);
	snprintf(MboxLock, sizeof(MboxLock), "%s%s", Mbox, LOCK_SUFFIX);
	if (lock_mbox(MboxLock))
		MboxLock[0] = '\0'; /* doesn't need a lockfile, maybe. */

	oflag = O_RDWR;
#if defined(O_EXLOCK)
	oflag |= O_EXLOCK;
#endif
	if ((srcfd = open(Mbox, oflag, 0)) < 0) {
		warning("open(%s) for rw/truncate", Mbox);  goto rerr;
	}
#if !defined(O_EXLOCK) && (HAVE_FLOCK || HAVE_LOCKF)
	if (flock(srcfd, LOCK_EX) < 0) {
		warning("flock(%s)", Mbox);  goto rerr;
	}
#endif
	if ((srcfp = fdopen(srcfd, FDREAD)) == NULL) {
		warning("fdopen(%s) for read", Mbox);  goto rerr;
	}

	while ((ln = Getline(srcfp)) != NULL) {
		if (Exit)
			goto werr;
		switch (state) {
		case ST_UNKNOWN:
			if (strncmp(ln, "From ", 5) == 0) {
				dstfp = open_new_inbox_file(&seq, inboxfile);
				if (dstfp == NULL)
					goto rerr;
				if (PreserveUnixFrom &&
				    fputs(ln, dstfp) == EOF) {
					warning("fputs(%s)", inboxfile);
					goto werr;
				}
				state = ST_HEADER;
			}
			break;
		case ST_HEADER:
			if (strlen(ln) < 3 &&
			    (ln[0] == '\n' || ln[0] == '\r')) {
				STRCPY(emptyline, ln);
				state = ST_BODY_AFTER_EMPTY_LINE;
				break;
			}
			if (fputs(ln, dstfp) == EOF) {
				warning("fputs(%s)", inboxfile);  goto werr;
			}
			if (UseCL &&
			    strncasecmp(ln, "Content-Length", 14) == 0) {
				int i;
				for (i = 14; i < strlen(ln); i++)
					if (isdigit((unsigned char)ln[i]))
						break;
				bytes = atoi(&ln[i]);
			}
			break;
		case ST_BODY_AFTER_EMPTY_LINE:
			if (bytes < 0 && strncmp(ln, "From ", 5) == 0) {
				if (fclose(dstfp))
					goto rerr;
				printf("%d\n", seq);

				dstfp = open_new_inbox_file(&seq, inboxfile);
				if (dstfp == NULL)
					goto rerr;
				if (PreserveUnixFrom &&
				    fputs(ln, dstfp) == EOF) {
					warning("fputs(%s)", inboxfile);
					goto werr;
				}
				state = ST_HEADER;
				break;
			}
			else if (fputs(emptyline, dstfp) == EOF)
				goto werr;
			/* FALLTHRU */
		case ST_BODY:
			if (strlen(ln) < 3 &&
			    (ln[0] == '\n' || ln[0] == '\r')) {
				STRCPY(emptyline, ln);
				state = ST_BODY_AFTER_EMPTY_LINE;
			}
			else
				state = ST_BODY;

			if (state == ST_BODY && fputs(ln, dstfp) == EOF)
				goto werr;
			if (bytes >= 0) {
				bytes -= strlen(ln);
				if (bytes <= 0) {
					if (fclose(dstfp))
						goto rerr;
					dstfp = NULL;
					printf("%d\n", seq);
					state = ST_UNKNOWN;
					bytes = -1;
					break;
				}
			}
			break;
		}
		free(ln);
	}
	if (dstfp) {
		if (fclose(dstfp))
			goto rerr;
		printf("%d\n", seq);
	}
	if (!Backup && ftruncate(srcfd, 0)) {
		unlock_mbox(MboxLock);
		error("ftruncate");
	}
	fclose(srcfp);
	unlock_mbox(MboxLock);
	return seq;

 werr:
	if (dstfp)
		fclose(dstfp);
	unlink(inboxfile);
 rerr:
	if (srcfp)
		fclose(srcfp);
	unlock_mbox(MboxLock);
	error("process_mbox(%s)", Mbox);
	return -1;		/* error. not reached */
}

private int
process_stdin(int seq)
{
	char inboxfile[PATH_MAX];
	char *ln;
	FILE *srcfp = stdin;
	FILE *dstfp;

	if ((dstfp = open_new_inbox_file(&seq, inboxfile)) == NULL)
		goto rerr;

	while ((ln = Getline(srcfp)) != NULL) {
		if (Exit)
			goto werr;
		if (fputs(ln, dstfp) == EOF) {
			warning("fputs(%s)", inboxfile);  goto werr;
		}
		free(ln);
	}

	if (dstfp) {
		fclose(dstfp);
		printf("%d\n", seq);
	}
	return seq;

 werr:
	if (dstfp)
		fclose(dstfp);
	unlink(inboxfile);
 rerr:
	error("process_stdin");
	return -1;		/* error. not reached */
}

private void
process(void)
{
	char mtimefile[PATH_MAX];
	FILE *fp;
	size_t wb;
	int len = strlen(MEW_MTIME_PHRASE);
	int seq = get_last_seq();
	int newseq = 0;

	switch (MboxType) {
	case T_MAILDIR:
		newseq = process_maildir(seq);
		break;
	case T_MBOX:
		newseq = process_mbox(seq);
		break;
	case T_STDIN:
		newseq = process_stdin(seq);
		break;
	default:
		error("unknown mbox type (%s)", Mbox);
	}

	/* update .mew-mtime file if new mail arrived */
	if (!CreateMTime || newseq <= seq)
		return;		/* no new mail */
	if (strlen(InboxDir) + strlen(MEW_MTIME_FILE) + 1 > PATH_MAX)
		error("pathname too long (%s%s)", InboxDir, MEW_MTIME_FILE);
	snprintf(mtimefile, sizeof(mtimefile), "%s/%s", InboxDir, MEW_MTIME_FILE);

	if ((fp = fopen(mtimefile, FDWRITE)) == NULL)
		error("can't create file (%s)", mtimefile);
	if ((wb = fwrite(MEW_MTIME_PHRASE, sizeof(char), len, fp)) != len) {
		fclose(fp);
		error("fwrite failed (%d, %s)", wb, mtimefile);
	}
	fclose(fp);
}

private void
sanity_check(void)
{
	struct stat sb;

	/* was directory exists? */
	if (stat(InboxDir, &sb))
		error("stat(%s)", InboxDir);
	if (!S_ISDIR(sb.st_mode) || access(InboxDir, W_OK))
		error("can't write directory (%s)", InboxDir);

	/* mbox type checking */
	if (MboxType == T_UNKNOWN &&
	    (MboxType = check_mbox_type(Mbox)) == T_UNKNOWN)
		error("can't find mbox (%s)", Mbox);
}

int
main(int argc, char **argv)
{
	extern char *Optarg;
	extern int Optind;
	char *progname = getprognm(argv[0]);
	int ch;

	warn_prog = progname;
	init_env(argc, argv);

	while ((ch = Getopt(argc, argv, "abcd:fhi:m:op:suvx:")) != EOF) {
		switch (ch) {
		case 'a':
			GetCur = TRUE;
			break;
		case 'b':
			Backup = TRUE;
			break;
		case 'c':
			UseCL = TRUE;
			break;
		case 'd':
		case 'm':
			if (strlen(Optarg) + 1 > PATH_MAX)
				error("pathname too long (%s)", Optarg);
			snprintf(Mbox, sizeof(Mbox), "%s", Optarg);
			break;
		case 'f':
			PreserveUnixFrom = TRUE;
			break;
		case 'i':
			STRDUP(InboxDir, Optarg);
			break;
		case 'o':
			Use_Suffix = TRUE;
			break;
		case 'x':
			STRDUP(Suffix, Optarg);
			Suffix_len = strlen(Suffix);
			break;
		case 'p':
			sscanf(Optarg, "%i", &FileMode);
			break;
		case 's':
			MboxType = T_STDIN;
			break;
		case 'u':
			CreateMTime = FALSE;
			break;
		case 'v':
			version(progname);
			exit(EXIT_SUCCESS);
		case 'h':
			help(progname);
			exit(EXIT_SUCCESS);
		default:
			usage(progname);
			exit(EXIT_SUCCESS);
		}
	}
	argc -= Optind;
	argv += Optind;

	sanity_check();
	process();
	return EXIT_SUCCESS;
}

/* 
 * Copyright (C) 2001-2005 Mew developing team.
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
 * incm.c ends here
 */
