# .emacs.d

Emacs configuration that attempts to be backwards compatible w/stock Emacs
installations on OS X, Debian (Squeeze), and RHEL 5.5.


## Installing

Modern Emacs comes with a package manager for installing third party Elisp
packages. This project includes an installation script that installs a set of
known packages using this package manager. If you are using an older version of
Emacs that is not supported by the package manager the installation script can
be omitted.


### Installing w/Emacs 24 or newer

1. Clone this repository to `~/.emacs.d`
2. Install external packages: `emacs -batch -l install.el`


### Installing w/Emacs 23

1. Clone this repository to `~/.emacs.d`

2. Download `package.el`

    See [How packages work in Emacs 23](http://www.emacswiki.org/emacs/ELPA#toc10)
    for a link to the current version of package.el that is compatible w/Emacs 23.
    Once you have downloaded the file, put it in `~/.emacs.d/site-lisp`.

3. Install external packages: `emacs -batch -l install.el`


## Extending

This repository is configured to exclude everything in `./site-lisp` and
`./elpa` from source contorl. This means that you can install packages using
elpa or by copying the elisp files into `./site-lisp` (which has been configued
to be on the load-path). All local configuration should go in
`site-lisp/default.el`.


### Local Customization Example

```elisp
;;; site-lisp/default.el --- Local Emacs customizations.
(setq user-mail-address "user@example.com"
      user-full-name "Your Name")


;; look & feel
(when window-system
  (load-theme 'wombat t)

  ;; custom font
  (set-face-attribute 'default nil :family "Source Code Pro")
  (set-face-attribute 'default nil :height 140)

  ;; set the window size & cursor color
  (setq default-frame-alist
        '((wait-for-wm . nil)
          (height . 45)
          (width . 120)
          (cursor-color . "Red"))) )


;; OS-X specific customizations
(when (eq system-type 'darwin)
  ;; configure path for external processes
  (setenv "PATH"
          (concat "/usr/texbin" ":" "/opt/local/bin" ":" "/usr/local/bin" ":" (getenv "PATH")))

  ;; configure path for subprocesses
  (setq exec-path
        (append exec-path '("/usr/texbin" "/opt/local/bin" "/usr/local/bin"))) )
```
