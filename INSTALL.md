# Installing Mew 6.9

## Platforms

Mew now supports Emacs 26.1 or later only.

## Installation

* Type `./configure`.

* Type `make`.

  * Mew's info is formatted by Emacs 26.3. So, the info could not be
     visible with other Emacsen. In this case, type `make info` to format
     the info.

  * If you want to format the Japanese info, type `make jinfo`.

*. Type `make install`. If you have the "install-info" command, the
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
