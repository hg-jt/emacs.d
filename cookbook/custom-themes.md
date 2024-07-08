# Using Custom Themes

## Problem

You want to use a custom theme in the GUI version of Emacs, but not in the TUI
version.


## Solution

To configure and enable a theme only in the GUI version of emacs, check for the
a non-nil value in `window-system`.

### Example 1: Built-in Theme

```lisp
(when window-system
  ;; set the theme
  (load-theme 'wombat t))
```


### Example 2: Dracula

```lisp
;; dracula w/normal size org headers
(add-hook 'after-init-hook
          (lambda ()
            (when (and window-system
                       (locate-library "dracula-theme"))
              ;; customize dracula
              (setq dracula-enlarge-headings nil                   ; normal sized headers
                    dracula-alternate-mode-line-and-minibuffer t)  ; alt mode line
              (load-theme 'dracula t)) ))
```


### Example 3: Gruvbox

```lisp
;; gruvbox w/custom cursor
(add-hook 'after-init-hook
          (lambda ()
            (when (locate-library "gruvbox-theme")
              (load-theme 'gruvbox-dark-hard t)
              (set-cursor-color "#fe8019")) ))
```
