# .emacs.d

Emacs configuration that attempts to be backwards compatible w/stock Emacs
installations on OS X, Debian (Wheezy), and RHEL 7.


## Installing

Modern Emacs comes with a built-in package manager for installing third party
Elisp packages. This project includes an installation script that installs a set
of known packages using the built-in package manager. If you are using an older
version of Emacs that is not supported by the package manager the installation
script can be omitted.


### Installing with Emacs 24 or newer

1. Clone this repository to `~/.emacs.d`

2. Ensure the correct `emacs` is on your path

    **OS X (with *Emacs For Mac OS X*)**:

    If you are using an OS X version prior to Catalina, you will need to ensure
    that the *Emacs For Mac OS X* version of emacs is on your path before the
    older version that is bundled with the OS.

        ln -s /Applications/Emacs.app/Contents/MacOS/Emacs /usr/local/bin/emacs

    > *NOTE*: This alias will launch the Emacs GUI when run from a terminal It
    > may also be useful to create an alias for running the Emacs CLI. To do
    > this, add the following to your shell configuration (e.g. `.bash_profile`,
    > `.zshrc`, etc.):
    >
    >     alias e='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'

3. Ensure your system has a CA certificate bundle

    **On OS X (with Emacs For Mac OS X)**

    Install *openssl* w/Homebrew (e.g. `brew install openssl`). This will
    install a the CA bundle in `/usr/local/etc/openssl/cert.pem`.

    > See https://stackoverflow.com/a/28274272 for alternative solutions to this
    > problem.

3. Install external packages

    Run `./install.el`

    This is a self-executing elisp script that will install the defined
    packages.

    > *NOTE*: If running this script does not work, you can remove the shebang
    > line and run it with emacs directly: `emacs -batch -l install.el`


### Installing w/Emacs 23

1. Clone this repository to `~/.emacs.d`

2. Download `package.el`

    See [How packages work in Emacs 23](http://www.emacswiki.org/emacs/ELPA#toc10)
    for a link to the current version of package.el that is compatible w/Emacs 23.
    Once you have downloaded the file, put it in `~/.emacs.d/site-lisp`.

3. Install external packages

    At this point, package installation should be the same as described above
    for Emacs 24 (or newer).


## Extending

This repository is configured to exclude everything in `./site-lisp` and
`./elpa` from source control. This means that you can install packages using
ELPA or by copying the elisp files into `./site-lisp` (which has been configued
to be on the load-path). All local configuration should go in
`site-lisp/default.el`.


### Local Customization Example

```elisp
;;; site-lisp/default.el --- Local Emacs customizations.

;; look & feel
(when window-system
  ;; set the window size & cursor color
  (setq default-frame-alist
        '((wait-for-wm . nil)
          (height . 45)
          (width . 140)
          (cursor-color . "Red"))
        split-width-threshold 140)

  ;; custom font
  (set-face-attribute 'default nil :family "Source Code Pro")
  (set-face-attribute 'default nil :height 140)

  ;; set the theme
  (load-theme 'wombat t))

;; personalization
(setq user-mail-address "user@example.com"
      user-full-name "Your Name")


;; OS-X specific customizations
(when (eq system-type 'darwin)
  (setq ns-use-thin-smoothing t)        ; better font smoothing

  ;; configure path for external processes
  (setenv "PATH"
          (concat "/usr/local/bin" ":" (getenv "PATH")))

  ;; configure path for subprocesses
  (setq exec-path
        (append exec-path '("/usr/local/bin"))) )
```
