# .emacs.d

Emacs configuration that attempts to be backwards compatible w/stock Emacs
installations on OS X, Debian (Squeeze), and RHEL 6.


## Installing

Modern Emacs comes with a built-in package manager for installing third party
Elisp packages. This project includes an installation script that installs a set
of known packages using the built-in package manager. If you are using an older
version of Emacs that is not supported by the package manager the installation
script can be omitted.


### Installing with Emacs 24 or newer

1. Clone this repository to `~/.emacs.d`

2. Ensure the correct `emacs` is on your path

    **On OS X (with *Emacs For Mac OS X*)**:

    Ensure that the *Emacs For Mac OS X* version of emacs is on your path before
    the old version that is bundled with the OS.

        ln -s /Applications/Emacs.app/Contents/MacOS/Emacs /usr/local/bin/emacs

    > *NOTE*: This alias will launch the Emacs GUI when run from a terminal
    > (except when run as a self-executing script). It may also be useful to
    > create an alias for running the CLI. To do this, add the following to your
    > `.bash_profile`:
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
`./elpa` from source contorl. This means that you can install packages using
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
          (concat "/usr/texbin" ":" "/opt/local/bin" ":" "/usr/local/bin" ":" (getenv "PATH")))

  ;; configure path for subprocesses
  (setq exec-path
        (append exec-path '("/usr/texbin" "/opt/local/bin" "/usr/local/bin"))) )
```


## Special Considerations

### Clearer Fonts on OS X (Emacs version < 26)

There are a few ways to clear up blurry fonts in OS X. So far, the best results
have been to use lightweight fonts. Here is an example using *Source Code Pro*:

```lisp
;; lightweight font
(set-face-attribute 'default nil :family "Source Code Pro" :weight 'light)
(set-face-attribute 'default nil :height 140)


;; workaround for blurry bold fonts on osx
(defun faces-bold-to-semibold ()
  "Switch bold font to semibold."
  (mapc
   (lambda (face)
     (when (eq (face-attribute face :weight) 'bold)
       (set-face-attribute face nil :weight 'normal)))
   (face-list)))

(when (eq system-type 'darwin)
  (mapc (lambda (mode)
          (eval-after-load mode #'faces-bold-to-semibold))
        '("markdown-mode" "org")))
```


### Using Custom Themes

```lisp
;; dracula w/normal size org headers
(add-hook 'after-init-hook
          (lambda ()
            (when (and window-system
                       (locate-library "dracula-theme"))
              ;; customize dracula
              (setq dracula-enlarge-headings nil  ; normal sized headers
                    dracula-alternate-mode-line-and-minibuffer t)  ; alt mode line
              (load-theme 'dracula t)) ))

;; gruvbox w/custom cursor
(add-hook 'after-init-hook
          (lambda ()
            (when (locate-library "gruvbox-theme")
              (load-theme 'gruvbox-dark-hard t)
              (set-cursor-color "#fe8019")) ))
```

### Using Flycheck w/Python

The configuration for using Flychexck w/Python makes a few assumptions, most
notably that you are using virtualenv and that you have defined a project-level
variable called `project-venv-name`. You can define this variable in a file
at the root of your project called `.dir-locals.el`. For example:

```elisp
;;; .dir-locals.el --- Project configuration.
((python-mode . ((project-venv-name . "project-x"))))
```


### Archiving org-mode entries

In some configurations, org-mode cannot find a function that it needs. The
workaround is to delete the compiled elc files. While non-ideal, this does
resolve the issue.

> *NOTE*: This specific error is
>
> ```
> org-copy-subtree: Invalid function: org-preserve-local-variables
> ```


```sh
cd ~/.emacs.d/elpa
find org-plus*/*.elc -print0 | xargs -0 rm
```


### Two-way SSL w/custom client certs with *Emacs for OSX*

Since certs are likely unique to your user/machine, the following configuration
should be added to `~/.emacs.d/site-lisp/default.el`, rather than the main
`~/.emacs.d/init.el` file.

1. Ensure you have the latet CA cert bundle

    Install *openssl* w/Homebrew (e.g. `brew install openssl`). This will
    install a the CA bundle in `/usr/local/etc/openssl/cert.pem`.

2. Add the latest CA bundle to the list of common trustfiles

    ```elisp
    (eval-after-load "gnutls"
      (lambda ()
        (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")))
    ```

3. Configure Emacs to ignore the built-in gnutls in favor of the command line app

    ```elisp
    (eval-after-load "tls"
      (lambda ()
        ;; force the use of external gnutls-cli for custom certs
        (when (fboundp 'gnutls-available-p)
          (defun gnutls-available-p () nil)) ))
    ```

4. Configure tls-program to use your custom certs

    In the same block as the previous step, add a custom string for invoking
    *gnutls-cli* with your custom certs.
    
    ```elisp
    (eval-after-load "tls"
      (lambda ()
        ;; force the use of external gnutls-cli for custom certs
        (when (fboundp 'gnutls-available-p)
          (defun gnutls-available-p () nil))

        ;; add custom gnutls-cli command
        (add-to-list 'tls-program
                     "gnutls-cli -p %p --x509cafile /path/to/ca/ca.pem --x509keyfile /path/to/key/key.pem --x509certfile /path/to/cert/public.pem %h"
                     t)))
    ```
