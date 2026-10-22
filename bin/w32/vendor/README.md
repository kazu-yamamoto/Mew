# Third-party sources for the Windows build

This directory is for sources that `Makefile.w32` compiles into
`mewl.exe` but that are not part of Mew.  It is empty except for this
file; git ignores the utf8proc files you put here.

## utf8proc (optional)

With utf8proc, the pick operators of `mewl` compare header fields by
the Unicode rules (see `INSTALL.md`).  To build it in:

1. Download the latest release of utf8proc (MIT license) from
   <https://github.com/JuliaStrings/utf8proc/> and put these three
   files here:

       utf8proc.h
       utf8proc.c
       utf8proc_data.c

2. In `Makefile.w32`, uncomment the two lines

       UTF8PROCFLAGS = -Iw32\vendor -DUTF8PROC_STATIC -DHAVE_UTF8PROC
       UTF8PROCOBJ   = utf8proc.obj

3. Build as usual.  `utf8proc.c` is compiled with the same flags as
   `mewl.c`, so no separate library or C runtime setting is needed.

`==` and `!==` then compare after NFC, and `=` and `!=` apply Unicode
full case folding and NFC.  To make `=` and `!=` also ignore
compatibility differences such as width and default ignorable
characters such as variation selectors (NFKC_Casefold), add
`-DUTF8PROC_CHAR_FOLD` to `UTF8PROCFLAGS`; this is what
`configure --enable-utf8proc-char-fold` does on Unix.

utf8proc 2.2 or later is required: it introduced `UTF8PROC_STATIC`,
which tells `utf8proc.h` that the functions are not in a DLL.  Nothing
else needs to be edited.
