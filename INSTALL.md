# Installing Mew 6.11

## Platforms

Mew now supports Emacs 27.1 or later only.

## What Mew is made of

Mew is Emacs Lisp and a handful of programs which the Lisp runs.  The
Lisp alone is not enough for everything:

| program | what needs it | written in |
|---|---|---|
| `mewl` | listing a local folder such as `+inbox`, and `pick`, `sort` and `join` | C |
| `incm` | taking mail in from an mbox or a maildir | C |
| `mewencode` | encoding and decoding MIME | C |
| `smew`, `cmew` | search and threading | Ruby |
| `mewest` | keeping a Hyper Estraier index | sh |
| `mew-pinentry` | asking for a passphrase | sh |

Reading IMAP alone asks less of these than you might expect, since the
summary of an IMAP folder is built from what the server says.  A local
folder is listed by `mewl`, and without it Mew says

```
'mewl' not found!
```

and shows nothing.  POP puts the mail it fetches into a local folder,
so POP needs `mewl` as well.

## Installation

* Type `./configure`.

* Type `make`.

  * Mew's info is formatted by one particular Emacs, so it may not be
    readable with another.  In that case, type `make info` to format it
    yourself.

  * If you want to format the Japanese info, type `make jinfo`.

* Type `make install`. If you have the "install-info" command, the
following line will be automatically added to the "dir" file.
Otherwise, add it by yourself.

```
* Mew: (mew.info).      Messaging in the Emacs World
```

* If you want the Japanese info, type `make install-jinfo`. If you
have the "install-info" command, the following line will be
automatically added to the "dir" file.  Otherwise, add it by yourself.

```
* Mew-J: (mew.ja.info).  Messaging in the Emacs World
```

* See info for configuring a site file/.emacs/.mew.el.
  See also ["conf/dot.emacs"](conf/dot.emacs) and
  ["conf/dot.mew"](conf/dot.mew).

## If you installed Mew from MELPA

MELPA carries the Emacs Lisp of Mew and none of the programs above, so
Mew will not get far on its own.  That recipe is not maintained here.

Build the programs from this repository and leave the Lisp to MELPA:

```sh
git clone https://github.com/kazu-yamamoto/Mew.git
cd Mew
./configure
make -C bin
sudo make -C bin install
```

`make -C bin` builds the programs and nothing else, so it does not
byte-compile any Lisp.  Note that `./configure` looks for Emacs even
here, since it also writes the Lisp side of the build, and stops with

```
configure: error: Emacs is not found
```

if it does not find one.

To install somewhere else, say `./configure --prefix=$HOME/.local`.  If
that place is not on the `PATH` Emacs sees, either add it to
`exec-path` or name the programs in full:

```elisp
(setq mew-prog-mewl "/path/to/mewl")
(setq mew-prog-mime-encode "/path/to/mewencode")
(setq mew-prog-mime-decode "/path/to/mewencode")
```

`smew` and `cmew` are Ruby scripts, so they need Ruby rather than a
compiler.
