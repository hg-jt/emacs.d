# .emacs.d

Emacs configuration.


## Installing

1. Clone this repository to `~/.emacs.d`.
2. Install external packages: `emacs -batch -l install.el`


## Extending

This repository is configured to ignore everything in `./site-lisp` and
`./elpa`. This means that you can install packages using elpa or by copying the
elisp files into `./site-lisp` (which has been configued to be on the
load-path). All local configuration should go in `site-lisp/default.el`.


### Local Customization Example (`site-lisp/default.el`)

```elisp
;;; default.el --- Local Emacs customizations.
(setq user-mail-address "user@example.com"
      user-full-name "Your Name")


;; look & feel
(when window-system
  (load-theme 'wombat t)

  ;; set the window size
  (setq default-frame-alist
        '((wait-for-wm . nil)
          (height . 45)
          (width . 120)
          (cursor-color . "Lime Green")))

  ;; custom font
  (when (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Source Code Pro")
    (set-face-attribute 'default nil :height 140)) )


;; configure PATH environment variable for external processes
(setenv "PATH"
        (concat "/usr/texbin" ":" "/usr/local/bin" ":" (getenv "PATH")))


;; add common OS-X third-party bin directories to exec-path
(setq exec-path
      (append exec-path '("/usr/texbin" "/opt/local/bin" "/usr/local/bin")))
```
