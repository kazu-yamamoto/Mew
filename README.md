# Messaging in the Emacs World

- Home page: [http://www.mew.org/](http://www.mew.org/)

- Installation: The github version is available via [MELPA](http://melpa.milkbox.net/) and the latest stable release is available via [MELPA Stable](http://melpa-stable.milkbox.net).

- Please read [INSTALL.md](INSTALL.md)

If you want to use Mew cloned from this repository, put the followings to your ".emacs".

```elisp
(setq mew-icon-directory (expand-file-name "SHOMEWHERE/mew/etc"))
(add-to-list 'image-load-path (expand-file-name "SHOMEWHERE/mew/etc"))
```