<div align="center"><img src="https://www.mew.org/image/Mew.jpg"/></div>
<h2 align="center">Mew - Messaging in the Emacs World</h2>

- Home page: [http://www.mew.org/](http://www.mew.org/)
- Installation: The github version is available via [MELPA](http://melpa.milkbox.net/) and the latest stable release is available via [MELPA Stable](http://melpa-stable.milkbox.net).

## Installation
Mew now supports **Emacs 24.3 or later** only.

### Manual installation
1. Clone
    ```
    git clone https://github.com/kazu-yamamoto/Mew
    ```
1. .configure
    ```
    .configure
    ```
1. make
    ```
    make
    ```
    - `make info` if you use other Emacsen other than GNU Emacs.
    - `make jinfo` if you need japanese info.
1. make install
    ```
    make install
    ```
    If you have the `install-info` command, the following line will be
    automatically added to the "dir" file.  Otherwise, add it by yourself.
    ```
    * Mew: (mew.info).      Messaging in the Emacs World
    ```
    - `make install-jinfo` if you need japanese info and you have `install-info`.
        ```
        * Mew-J: (mew.ja.info).  Messaging in the Emacs World
        ```
1. Sample config

    See also `dot.emacs` and `dot.mew` to configure mew.
