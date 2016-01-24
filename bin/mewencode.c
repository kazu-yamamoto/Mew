/*
 * mewencode, mewdecode, and mewcat --- MIME encoding for Mew
 *
 * Author:  Kazu Yamamoto <Kazu@Mew.org>
 * Created: Dec  8, 1994
 *
 * Code:
 */

#include "mew.h"

private char version_message[] = "version 6.6 20140416 Kazu Yamamoto";

#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef HAVE_LIBZ
#include <zlib.h>
#endif

#define ENCODE_NAME "mewencode"
#define DECODE_NAME "mewdecode"
#define CAT_NAME    "mewcat"

#define ENCODE   1
#define DECODE   2
#define CHECK8   8

#define LINELEN 70

#define BASE64	'b'
#define QP     	'q'
#define GZIP64	'z'
#define UU	'u'

#define OOB -1
#define EOP -2
#define ELF -3
#define ECR -4

typedef unsigned char byte;

/****************************************************************
 *
 * prototype
 *
 */

private void usage(const char *);
private void help(const char *);
private void version(const char *);
private int  read_text(byte *, unsigned int);
private int  read_binary(byte *, unsigned int);
#ifdef HAVE_LIBZ
private int  deflate_gzip(byte *, unsigned int);
#endif /* HAVE_LIBZ */
private int  encode_base64(byte *, unsigned int);
private void write_base64(void);
private void base64_encode(int, int);
private void gzip64_encode(int, int);
private int  read_base64(byte *, unsigned int);
private int  decode_base64(byte *, unsigned int);
#ifdef HAVE_LIBZ
private int  inflate_gzip(byte *, unsigned int);
#endif /* HAVE_LIBZ */
private void write_text(void);
private void write_binary(void);
private void base64_decode(int);
private void gzip64_decode(int);
private void quoted_printable_encode(int);
private int  puthexchar(int, int);
private void quoted_printable_decode(void);
private void uu_decode(int);
private void check_8bit(void);

/****************************************************************
 *
 * long name convention for option
 *
 */

private void
usage(const char *progname)
{
	fprintf(stderr, "Usage: %s [-e|-d|-8] [-b|-q|-g|-u] [-l length] [-t] [infile [outfile]]\n", progname);
}

private const char *
help_message[] = {
	" -e Encoding <infile>",
	" -d Decoding <infile> instead of encoding",
	"    Decoding is the default when called with the decoding",
	"    program name.", 
	" -b MIME base64 en/decoding.",
	" -q MIME quoted-printable en/decoding.",
	" -g MIME gzip64 en/decoding(not yet specified in RFC).",
	" -z The same as -g.",
	" -u Uudecoding.",
	" -l Line length into which base64/quoted-printable/gzip64 ",
	"    encoding truncate. The default value is 70.",
	" -t On base64/gzip64 encoding, local newline is treated as CRLF.",
	"    On base/gzip64 decoding,any newline is translated",
	"    into local newline.",
	"    Specify this option only when the input is a line",
	"    based object(e.g. Content-Type: is text/plain or",
	"    application/postscript).",
	" -8 See if any 8bit characters are contained.",
	" -h Display this help message.",
	" -v Display the version.",
	"",
	"Default is Encoding, Base64, Length = 70, Binary.",
	"If <infile> is \"-\", it means the standard input.",
	"If <outfile> is \"-\", it means the standard output.",
	"If <outfile> is given for uudecoding, an embedded file is used.",
	NULL
};

private void
help(const char *progname)
{
	const char **p = help_message;

	fprintf(stderr, "Help: %s\n\n", progname);
	fprintf(stderr, " MIME encoder/decoder.\n\n");
	usage(progname);
	while (*p) fprintf(stderr, "%s\n", *p++);
}

private void
version(const char *progname)
{
	fprintf(stderr, "%s %s\n", progname, version_message);
}

/****************************************************************
 *
 * Base64 / Gzip64 encoder
 *
 */

struct {
	int (*e_func1)(byte *, unsigned int);
	int (*e_func2)(byte *, unsigned int);
	int (*e_func3)(byte *, unsigned int);
	unsigned int e_length;
} E_ctx;

#if HAVE_LIBZ
private byte e_buf1[BUFSIZ];
#endif  /* HAVE_LIBZ */
private byte e_buf2[BUFSIZ];
private byte e_buf3[BUFSIZ];

private char
base64[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

private int
read_text(byte *p, unsigned int max)
{
	static int done = NO;
	static int cr_in = NO;
	int c;
	byte *start = p, *lim = p + max;

	if (done) return 0;

	while (p < lim) {
		c = getchar();
    		if (cr_in == YES) {
			cr_in = NO;	
			*p = LF;
			switch (c) {
			case EOF:
				done = YES;
				return p - start;
			case LF:
				break;
			case CR:
			default:
				ungetc(c, stdin);
				break;
			}
		} else {
			switch (c) {
			case EOF:
				done = YES;
				return p - start;
			case LF:
				*p = CR;
				cr_in = YES;				
				ungetc(c, stdin);
				break;
			case CR:
				*p = CR;
				cr_in = YES;
				break;
			default:
				*p = c;
				break;
			}
		}
		p++;
	}
	return p - start;
}

private int
read_binary(byte *p, unsigned int max)
{
	return fread(p, sizeof(byte), max, stdin);
}

#ifdef HAVE_LIBZ
private int
deflate_gzip(byte *p, unsigned int max)
{
	static int done = NO;
	static int initialized = NO;
	static int finalize = NO;	
	static uLong crc = 0;
	static uLong total = 0;
	static z_stream z;
	int err, size, deflated = 0, old;
	byte *q;

	if (done == YES) return 0;

	if (initialized == NO) {
		initialized = YES;

		crc = crc32(0L, Z_NULL, 0);
		z.zalloc = (alloc_func)0;
		z.zfree = (free_func)0;
		z.opaque = (voidpf)0;
		z.avail_in = 0;

		err = deflateInit2(&z, Z_DEFAULT_COMPRESSION,
				   Z_DEFLATED, -MAX_WBITS,
				   8, Z_DEFAULT_STRATEGY);
		if (err != Z_OK) warn_exit("deflateInit2 error.");
		*p++ = 0x1f;
		*p++ = 0x8b;
		*p++ = Z_DEFLATED;
		*p++ = 0; /* flag */
		*p++ = 0; /* mtime */
		*p++ = 0; /* mtime */
		*p++ = 0; /* mtime */
		*p++ = 0; /* mtime */
		*p++ = 0; /* xf */
		*p++ = 3; /* UNIX */
		return 10;
	}

	z.next_out = p;
	z.avail_out = max;

	if (finalize == YES) goto finalize;

	while (z.avail_out != 0) {
		if (z.avail_in == 0) {
			size = (*E_ctx.e_func1)(e_buf1, sizeof(e_buf1));
			if (size == 0) {
				finalize = YES;
				if (deflated == 0) goto finalize;
				return deflated;
			}
			z.next_in  = e_buf1;
			z.avail_in = size;
			total += size;
			crc = crc32(crc, e_buf1, (uInt)size);
		}

		old = z.avail_out;
		err = deflate(&z, Z_SYNC_FLUSH);

		if (err != Z_OK && err != Z_STREAM_END)
			warn_exit("inflate error (%d).", err);

		deflated = max - z.avail_out;		

		if (old == z.avail_out)
			break;
	}
	return deflated;

 finalize:
	done = YES;
	
	do {
		err = deflate(&z, Z_FINISH);
	} while (err != Z_STREAM_END);
		
	if (deflateEnd(&z) != Z_OK)
		warn_exit("deflateEnd error.");

	deflated = max - z.avail_out;
	q = z.next_out;
	*q++ = (int)(crc & 0xff);
	*q++ = (int)((crc >>= 8) & 0xff);
	*q++ = (int)((crc >>= 8) & 0xff);
	*q++ = (int)((crc >>= 8) & 0xff);
	*q++ = (int)(total & 0xff);
	*q++ = (int)((total >>= 8) & 0xff);
	*q++ = (int)((total >>= 8) & 0xff);
	*q++ = (int)((total >>= 8) & 0xff);
	deflated += 8;
	return deflated;
}
#endif /* HAVE_LIBZ */

private int
encode_base64(byte *p, unsigned int max)
{
	static int done = NO;
	static unsigned int avail = 0;
	static byte *q;
	unsigned int i, size, out = 0;
	byte c1, c2, c3, *r, *lim = p + max;

	if (done == YES) return 0;

	while (p + 4 < lim) {
		if (avail < 3) {
			r = q;
			q = e_buf2;
			for (i = 0; i < avail; i++)
				*q++ = *r++;
			size = (*E_ctx.e_func2)(q, sizeof(e_buf2) - avail);
			q = e_buf2;
			avail += size;
			if (size == 0) {
				done = YES;
				switch (avail) {
				case 0:
					return out;
				case 1:
					c1 = *q++;
					*p++ = base64[c1 >> 2];
					*p++ = base64[((c1 & 0x3) << 4)];
					*p++ = EQ;
					*p++ = EQ;
					return out + 4;
				case 2:
					c1 = *q++;
					c2 = *q++;
					*p++ = base64[c1 >> 2];
					*p++ = base64[((c1 & 0x3) << 4) |
						     ((c2 & 0xf0) >> 4)];
					*p++ = base64[((c2 & 0xf) << 2)];
					*p++ = EQ;
					return out + 4;
				}
			}
		}
		if (avail >= 3) {
			c1 = *q++;
			c2 = *q++;
			c3 = *q++;
			*p++ = base64[c1 >> 2];
			*p++ = base64[((c1 & 0x3) << 4) | ((c2 & 0xf0) >> 4)];
			*p++ = base64[((c2 & 0xf) << 2) | ((c3 & 0xc0) >> 6)];
			*p++ = base64[c3 & 0x3f];
			avail -= 3;
			out += 4;
		}
	}
	return out;
}

private void
write_base64()
{
	unsigned int length = E_ctx.e_length;
	unsigned int size = 0, out = 0, gap;
	byte *p;

	while ((size = (*E_ctx.e_func3)(e_buf3, sizeof(e_buf3))) != 0) {
		p = e_buf3;
		if (out != 0) {
			gap = length - out;
			if (size < gap) {
				fwrite(p, sizeof(byte), size, stdout);
				out += size;
				continue;
			}
			fwrite(p, sizeof(byte), gap, stdout);
			p += gap;
			size -= gap;
			putchar(LF);
			out = 0;
		}
		
		while (size >= length) {
			fwrite(p, sizeof(byte), length, stdout);
			p += length;
			size -= length;
			putchar(LF);
		}

		fwrite(p, sizeof(byte), size, stdout);
		out = size;
	}

	if (out != 0) putchar(LF);
}
       
private void
base64_encode(int text, int length)
{
	E_ctx.e_func1 = NULL;
	if (text == YES)
		E_ctx.e_func2 = read_text;
	else
		E_ctx.e_func2= read_binary;
	E_ctx.e_func3 = encode_base64;
	E_ctx.e_length = length;
	write_base64();
}

private void
gzip64_encode(int text, int length)
{
#ifdef HAVE_LIBZ
	if (text == YES)
		E_ctx.e_func1 = read_text;
	else
		E_ctx.e_func1= read_binary;
	E_ctx.e_func2 = deflate_gzip;
	E_ctx.e_func3 = encode_base64;
	E_ctx.e_length = length;
	write_base64();
#else /* HAVE_LIBZ */
	warn_exit("not linked to zlib.");
#endif /* HAVE_FORK */
}

/****************************************************************
 *
 * Base64 / Gzip64 decoder
 *
 */

struct {
	int (*d_func1)(byte *, unsigned int);
	int (*d_func2)(byte *, unsigned int);
	int (*d_func3)(byte *, unsigned int);
} D_ctx;

private byte d_buf1[BUFSIZ];
#if HAVE_LIBZ
private byte d_buf2[BUFSIZ];
#endif /* HAVE_LIBZ */
private byte d_buf3[BUFSIZ];

private signed char
base256[] = {
	OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,ELF,OOB, OOB,ECR,OOB,OOB,
    
	OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB,
      /*                                                -                / */
	OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB, 62, OOB,OOB,OOB, 63,
      /*  0   1   2   3    4   5   6   7    8   9                =        */
	 52, 53, 54, 55,  56, 57, 58, 59,  60, 61,OOB,OOB, OOB,EOP,OOB,OOB,
      /*      A   B   C    D   E   F   G    H   I   J   K    L   M   N   O*/
	OOB,  0,  1,  2,   3,  4,  5,  6,   7,  8,  9, 10,  11, 12, 13, 14,
      /*  P   Q   R   S    T   U   V   W    X   Y   Z                     */
	 15, 16, 17, 18,  19, 20, 21, 22,  23, 24, 25,OOB, OOB,OOB,OOB,OOB,
      /*      a   b   c    d   e   f   g    h   i   j   k    l   m   n   o*/
	OOB, 26, 27, 28,  29, 30, 31, 32,  33, 34, 35, 36,  37, 38, 39, 40,
      /*  p   q   r   s    t   u   v   w    x   y   z                     */
	 41, 42, 43, 44,  45, 46, 47, 48,  49, 50, 51,OOB, OOB,OOB,OOB,OOB, 
};

private int
read_base64(byte *p, unsigned int max)
{
	static int done = NO, cnt = 0;
	int c, d, out = 0, lf = 0, cr = 0;
	byte *lim = p + max;

	if (done == YES) return 0;

	while (p < lim) {
		c = getchar();
		if (c == EOF) {
			done = YES;
			return out;
		}
		d = base256[c];
		switch (d) {
		case OOB:
			warning("invalid base64 character.");
			goto skiptail;
		case EOP:
			goto skiptail;
		case ELF:
			/* If cnt == 0, unnecessary null lines are
			   prepended. */
			if (++lf >= 2 && cnt > 0) goto skiptail;
			continue;
		case ECR:
			if (++cr >= 2 && cnt > 0) goto skiptail;
			continue;
		}
		lf = cr = 0;
		*p++ = d;
		out++;
		cnt++;
	}
	return out;
 skiptail:
	done = YES;
	return out;
}

private int
decode_base64(byte *p, unsigned int max)
{
	static int done = NO;
	static unsigned int avail = 0;
	static byte *q;
	unsigned int i, size, out = 0;
	byte c1, c2, c3, c4, *r, *lim = p + max;

	if (done == YES) return 0;

	while (p + 3 < lim) {
		if (avail < 4) {
			r = q;
			q = d_buf1;
			for (i = 0; i < avail; i++)
				*q++ = *r++;
			size = (*D_ctx.d_func1)(q, sizeof(d_buf1) - avail);
			q = d_buf1;
			avail += size;
			if (size == 0) {
				done = YES;
				switch (avail) {
				case 0:
					return out;
				case 1:
					warning("invalid base64 length.");
					return out; /* anyway */
				case 2:
					c1 = *q++;
					c2 = *q++;
					*p++ = (c1 << 2) | ((c2 & 0x30) >> 4);
					return out + 1;
				case 3:
					c1 = *q++;
					c2 = *q++;
					c3 = *q++;
					*p++ = (c1 << 2) | ((c2 & 0x30) >> 4);
					*p++ = ((c2 & 0x0f) << 4) |
						((c3 & 0x3c) >> 2);
					return out + 2;
				}
			}
		}

		if (avail >= 4) {
			c1 = *q++;
			c2 = *q++;
			c3 = *q++;
			c4 = *q++;
			*p++ = (c1 << 2) | ((c2 & 0x30) >> 4);
			*p++ = ((c2 & 0x0f) << 4) | ((c3 & 0x3c) >> 2);
			*p++ = ((c3 & 0x03) << 6) | c4;
			avail -= 4;
			out += 3;
		}
	}
	return out;
}

#ifdef HAVE_LIBZ
private int
inflate_gzip(byte *p, unsigned int max)
{
	static int done = NO;
	static int initialized = NO;
	static z_stream z;
	int err, size, inflated = 0, old;

	if (done == YES) return 0;

	if (initialized == NO) {
		byte flag = 0;
		uInt len;
		byte *q, *r;

		initialized = YES;
		z.zalloc = (alloc_func)0;
		z.zfree = (free_func)0;
		z.opaque = (voidpf)0;
		err = inflateInit2(&z, -MAX_WBITS);
		if (err != Z_OK) warn_exit("inflateInit2 error.");
		size = (*D_ctx.d_func2)(d_buf2, sizeof(d_buf2));
		q = d_buf2;
		r = q + size;
		if (*q++ != 0x1f) warn_exit("not gzip format (ID1).");
		if (*q++ != 0x8b) warn_exit("not gzip format (ID2).");
		if (*q++ != Z_DEFLATED) warn_exit("not gzip format (CM).");
		flag = *q++;
		q += 6; /* mtime, xf, os */

		/*
		 * Optional header, probably omitted.
		 */
		if (flag & 0x04) { /* xlen */
			len = *q++;
			len = len + (*q++ << 8);
			q += len;
		}
		if (flag & 0x08) /* file name */
			while (*q++ != NUL);
			
		if (flag & 0x10) /* comment */
			while (*q++ != NUL);

		if (flag & 0x02) /* crc */
			q += 2;
		
		z.next_in  = q;
		z.avail_in = r - q;
	}

	z.next_out = p;
	z.avail_out = max;

	while (z.avail_out != 0) {
		if (z.avail_in == 0) {
			size = (*D_ctx.d_func2)(d_buf2, sizeof(d_buf2));
			z.next_in  = d_buf2;
			z.avail_in = size;
		}

		old = z.avail_out;
		err = inflate(&z, Z_SYNC_FLUSH);

		if (err != Z_OK && err != Z_STREAM_END)
			warn_exit("inflate error (%d).", err);

		inflated = max - z.avail_out;		

		if (old == z.avail_out)
			break;

		if (err == Z_STREAM_END) {
			done = YES;
			/* 8 bytes (crc and isize) are left. */
			if (inflateEnd(&z) != Z_OK)
				warn_exit("inflateEnd error.");
			break;
		}
	}

	return inflated;
}
#endif /* HAVE_LIBZ */

private void
write_text()
{
	int i, c, size, cr_out = NO;
	
	while ((size = (*D_ctx.d_func3)(d_buf3, sizeof(d_buf3))) != 0) {
		for (i = 0; i < size; i++) {
			c = d_buf3[i];
			if (cr_out == YES) {
				cr_out = NO;
				switch (c) {
				case LF : 
					break;
				case CR : 
					putchar(LF);
					cr_out = YES;
					break;
				default:
					putchar(c);
					break;
				}
				continue;
			}
		
			switch (c) {
			case CR : 
				putchar(LF);
				cr_out = YES;
				break;
			default:
				putchar(c);
				break;
			}
		}
	}
}

private void
write_binary()
{
	unsigned int size;

	while ((size = (*D_ctx.d_func3)(d_buf3, sizeof(d_buf3))) != 0)
		fwrite(d_buf3, sizeof(byte), size, stdout);
}

private void
base64_decode(int text)
{
	D_ctx.d_func1 = read_base64;
	D_ctx.d_func2 = NULL;
	D_ctx.d_func3 = decode_base64;
	if (text == YES)
		write_text();
	else
		write_binary();
}

private void
gzip64_decode(int text)
{
#ifdef HAVE_LIBZ
	D_ctx.d_func1 = read_base64;
	D_ctx.d_func2 = decode_base64;
	D_ctx.d_func3 = inflate_gzip;
	if (text == YES)
		write_text();
	else
		write_binary();
#else /* HAVE_LIBZ */
	warn_exit("not linked to zlib.");
#endif /* HAVE_LIBZ */
}
/****************************************************************
 *
 * Quoted_Printable
 *
 */

private char base16[] = "0123456789ABCDEF";
private char From[] = "\nFrom ";

#define softbreak() {putchar(EQ); putchar(LF);}

private void
quoted_printable_encode(int length)
{
	int c, len = 0, sp = NO, lfdot = NO, Fromix = 1;

	while ((c = getchar()) != EOF) {
		if ((c == TAB) || (c == SP)) {
			if (From[Fromix] == c) { /* SP */
				putchar(EQ);
				putchar(base16[c >> 4]);
				putchar(base16[c & 0x0f]);
				len += 3;
				Fromix = 0;
				continue;
			}
			Fromix = 0;
			sp = YES;
			putchar(c);
			if ((++len) > length) {
				sp = NO;
				len = 0;
				lfdot = LF;
				Fromix = 1;
				softbreak();
			}
			continue;
		}
		if (c == LF) {
			if (sp || (lfdot == DOT))
				softbreak();
			len = 0;
			sp = NO;
			lfdot = LF;
			Fromix = 1;
			putchar(LF);
			continue;
		}
		if ((c < SP) || (c == EQ) || (c >= DEL)) {
			/* exclusive TAB, SP */
			sp = NO;
			putchar(EQ);
			putchar(base16[c >> 4]);
			putchar(base16[c & 0x0f]);
			len += 3;
			if (len > length) {
				len = 0;
				lfdot = LF;
				Fromix = 1;
				softbreak();
			} else {
				Fromix = 0;
				lfdot = NO;
			}
			continue;
		}
		sp = NO;

		if (From[Fromix] == c)
			Fromix++;
		else
			Fromix = 0;
	
		if (c == DOT && lfdot == LF)
			lfdot = DOT; 
		else
			lfdot = NO;

		putchar(c);
		if ((++len) > length) {
			len = 0;
			lfdot = LF;
			Fromix = 1;
			softbreak();
		}
	}
	if (len > 0) softbreak(); /* ignored by decoder */
}

private signed char
base128[] = {
	OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB,
	OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB,
	OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB,
	  0,  1,  2,  3,   4,  5,  6,  7,   8,  9,OOB,OOB, OOB,OOB,OOB,OOB,
	OOB, 10, 11, 12,  13, 14, 15,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB,
	OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB,
	OOB, 10, 11, 12,  13, 14, 15,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB,
	OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB,
};

private int
puthexchar(int c1, int c2)
{
	int a1, a2;
	static int warned = NO;

	if (((a1 = base128[c1]) != OOB) && ((a2 = base128[c2]) != OOB)) {
		putchar(((a1 << 4) | a2));
		return(TRUE);
	} else {
		if (warned == NO) { /* warn just once */
			warning("can't translate hex to character: %c%c", c1, c2);
			warned = YES;
		}
		return(FALSE);	
	}
}

private void
quoted_printable_decode()
{
	int c1, c2, c3;

	/* if an error occurs, print input sequence as it is, anyway, sigh */
    
	while((c1 = getchar()) != EOF) {
	skipgetc:
		if (c1 == EQ) {
			if ((c2 = getchar()) == EOF) {
				putchar(EQ);
				warn_exit("end of file after =.");
			}
			if (c2 == LF) continue;
			if (c2 == CR) {
				if ((c3 = getchar()) == EOF) break;
				if (c3 == LF) {
					continue;
				} else {
					ungetc(c3, stdin);
					continue;
				}

			}
			if ((c3 = getchar()) == EOF) {
				putchar(EQ);
				putchar(c2);
				warn_exit("end of file after =.");
			}
			if (puthexchar(c2, c3) == FALSE) {
				putchar(EQ);
				if ((c1 = c2) == EOF)
					warn_exit("invalid eof.");
				ungetc(c3, stdin);
				goto skipgetc;
			} else
				continue;
		}
		putchar(c1);
	}
}

/****************************************************************
 *
 * uudecode
 *
 */

private char Uu_read_buf[BUFSIZ];
private char Uu_flnm_buf[BUFSIZ];

#define uuchar(c) (((c) - SP) & 0x3f)

private void
uu_decode(int use_file_name)
{
	int n, c1, c2, c3, c4;
	char *p, *q;

	while (fgets(Uu_read_buf, sizeof(Uu_read_buf), stdin) != NULL) {
		if (STRCMP(Uu_read_buf, "begin ") == 0) {
			if (use_file_name == YES) {
				if ((p = strchr(Uu_read_buf + 6, SP)) == NULL)
					warn_exit("no filename for uudecode.");
				p++;
				for (q = Uu_flnm_buf; *p != LF; p++, q++)
					*q = *p;
				if (freopen(Uu_flnm_buf, FDWRITE, stdout) == NULL)
					warn_exit("can't open a file.");
			}
			break;
		}
	}

	while (fgets(Uu_read_buf, sizeof(Uu_read_buf), stdin) != NULL) {
		if (STRCMP(Uu_read_buf, "end") == 0)
			break;
		p = Uu_read_buf;
		n = uuchar(*p++);

		if (((n + 2) / 3) != (strlen(Uu_read_buf) / 4))
			continue;

		while (n > 0) {
			if (n >= 3) {
				c1 = uuchar(*p++);
				c2 = uuchar(*p++);
				c3 = uuchar(*p++);
				c4 = uuchar(*p++);
				putchar(((c1 << 2) | ((c2 & 0x30) >> 4)));
				putchar((((c2 & 0x0f) << 4) |
					 ((c3 & 0x3c) >> 2)));
				putchar((((c3 & 0x03) << 6) | c4));
			} else if (n == 2) {
				c1 = uuchar(*p++);
				c2 = uuchar(*p++);
				c3 = uuchar(*p++);
				putchar(((c1 << 2) | ((c2 & 0x30) >> 4)));
				putchar((((c2 & 0x0f) << 4) |
					 ((c3 & 0x3c) >> 2)));
			} else {
				c1 = uuchar(*p++);
				c2 = uuchar(*p++);
				putchar(((c1 << 2) | ((c2 & 0x30) >> 4)));
			}
			n = n - 3;
		}
	}
	if (use_file_name == YES)
		fprintf(stderr, "filename: %s\n", Uu_flnm_buf);
}

/****************************************************************
 *
 * Checking 8bit
 *
 */

private void
check_8bit()
{
	int c;
	while ((c = getchar()) != EOF)
		if (c & 0x80) {
			printf("8bit\n");
			exit(EXIT_FAILURE);
		}
	printf("7bit\n");
	exit(EXIT_SUCCESS);
}

/****************************************************************
 *
 * main
 *
 */

int
main(int argc, char **argv)
{
	int optc, file_count;
	/*
	 * default option values
	 */
	int  action = ENCODE;
	char encode = BASE64;	/* -b -q -g */
	int  length = LINELEN;	/* -l num */
	int  text   = NO;	/* -t */
	int  use_file_name = NO;
	char *progname = getprognm(argv[0]);

	warn_prog = progname;

	while((optc = Getopt(argc, argv, "esdbqgzl:tuhv8")) != EOF) {
		switch (optc) {
		case 'e':
			action = ENCODE;
			break;
		case 'd':
			action = DECODE;
			break;
		case 'b':
			encode = BASE64;
			break;
		case 'q':
			encode = QP;
			break;
		case 'g':
		case 'z':
			encode = GZIP64;
			break;
		case 'u':
			encode = UU;
			break;
		case '8':
			action = CHECK8;
			break;
		case 'l':
			length = atoi(Optarg);
			if (length == 0) length = LINELEN;
			break;
		case 't':
			text = YES;
			break;
		case 'h':
			help(progname);
			exit(EXIT_SUCCESS);
			break;
		case 'v':
			version(progname);
			exit(EXIT_SUCCESS);
			break;
		default:
			usage(progname);
			exit(EXIT_FAILURE);
		}
	}

	file_count = argc - Optind;

	/* Override arguments by progname. */

	if (action == ENCODE) {
		if (STRCMP(progname, DECODE_NAME) == 0)
			action = DECODE;
		if (STRCMP(progname, CAT_NAME) == 0) {
			if (file_count != 0)
				warn_exit("too many arguments.");
			action = DECODE;
		}
	}

	switch(file_count) {
	case 0:
		break;
	case 1:
		if (STRCMP(argv[Optind], "-") == 0)
			;
		else if (freopen(argv[Optind], FDREAD, stdin) == NULL)
			warn_exit("can't open file %s.", argv[Optind]);
		break;
	case 2:
		if (STRCMP(argv[Optind], "-") == 0)
			;
		else if (freopen(argv[Optind], FDREAD, stdin) == NULL)
			warn_exit("can't open file %s.", argv[Optind]);
		Optind++;
		if (STRCMP(argv[Optind], "-") == 0)
			;
		else if (encode == UU)
			use_file_name = YES;
		else if (freopen(argv[Optind], FDWRITE, stdout) == NULL)
			warn_exit("can't open file %s.", argv[Optind]);
		break;
	default:
		usage(progname);
		exit(EXIT_FAILURE);
		break;
	}

	SET_BINARY_MODE(stdin);
	SET_BINARY_MODE(stdout);

	switch (action) {
	case ENCODE:
		switch (encode) {
		case BASE64:
			base64_encode(text, length);
			break;
		case QP:
			quoted_printable_encode(length);
			break;
		case GZIP64:
			gzip64_encode(text, length);
			break;
		}
		break;
	case DECODE:
		switch (encode) {
		case BASE64:
			base64_decode(text);
			break;
		case QP:
			quoted_printable_decode();
			break;
		case GZIP64:
			gzip64_decode(text);
			break;
		case UU:
			uu_decode(use_file_name);
			break;
		}
		break;
	case CHECK8:
		check_8bit();
		break;
	}

	fclose(stdin);
	fclose(stdout);
	exit(EXIT_SUCCESS);
}

/* 
 * Copyright (C) 1994-2005 Mew developing team.
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
 * mewencode.c ends here
 */
