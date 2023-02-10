# <Readme for Mew 6.8

- Kazu Yamamoto
- Feb 10, 2023

This is stable releases of Mew 6.8.

## Platforms

Mew now supports Emacs 26.1 or later only.

## Installation

1. Type `./configure`.
2. Type `make`.
  1. Mew's info is formatted by Emacs 27.x. So, the info could not be
     visible with other Emacsen. In this case, type `make info` to format
     the info.
  2. If you want to format the Japanese info, type `make jinfo`.
3. Type `make install`.

If you have the "install-info" command, the following line will be
automatically added to the "dir" file.  Otherwise, add it by yourself.

```
* Mew: (mew.info).      Messaging in the Emacs World
```

4. If you want the Japanese info, type `make install-jinfo`. If you
have the "install-info" command, the following line will be
automatically added to the "dir" file.  Otherwise, add it by yourself.

```
* Mew-J: (mew.ja.info).  Messaging in the Emacs World
```

5. See info for configuring a site file/.emacs/.mew.el.
    See also ["conf/dot.emacs"](conf/dot.emacs) and ["conf/dot.mew"](conf/dot.mew).
