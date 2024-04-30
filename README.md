# Mew (Messaging in the Emacs World)

Mew is a mail-reader on Emacs, which is free to use and whose source code is open.

## Privacy policy

Users need to configure their e-mail addresses and e-mail servers but such information is not sent anywhere. Users are not trucked at all.

## Information

- Home page: [http://www.mew.org/](http://www.mew.org/)

- Installation: The github version is available via [MELPA](http://melpa.milkbox.net/) and the latest stable release is available via [MELPA Stable](http://melpa-stable.milkbox.net).

- Installation by hand: [INSTALL.md](INSTALL.md)

- Developers: if you want to use Mew cloned from this repository, put the followings to your ".emacs".

```elisp
(setq mew-icon-directory (expand-file-name "SHOMEWHERE/mew/etc"))
(add-to-list 'image-load-path (expand-file-name "SHOMEWHERE/mew/etc"))
```

