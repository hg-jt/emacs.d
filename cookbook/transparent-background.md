# Transparent Background

## Problem

You want to a toggle to enable the transparent background feature introduced in
Emacs 29.

> ***NOTE*** This configuring using the older `alpha` frame parameter rather
> than the newer `alpha-background`. YMMV based on OS and Emacs version. You may
> need to adjust `jt/set-transparency` accordingly.
>
> Tested on Emacs 30.1 on macOS.

## Solution

```lisp
(defvar jt/transparency-level 92
  "Transparency level for Emacs background (0-100).")

(defvar jt/transparency-enabled nil
  "Whether transparency is currently enabled.")

(defun jt/set-transparency (value)
  "Set transparency VALUE for all frames."
  (dolist (frame (frame-list))
    (set-frame-parameter frame 'alpha (cons value value))))

(defun jt/toggle-transparency ()
  "Toggle background transparency for the current frame."
  (interactive)
  (if jt/transparency-enabled
      (progn
        (jt/set-transparency 100)
        (setq jt/transparency-enabled nil)
        (message "Transparency disabled"))
    (jt/set-transparency jt/transparency-level)
    (setq jt/transparency-enabled t)
    (message "Transparency enabled (%d%%)" jt/transparency-level)))

;; (optional) set global keybinding to toggle transparency
(global-set-key (kbd "C-c t") #'jt/toggle-transparency)
```
