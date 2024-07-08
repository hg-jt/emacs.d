# Clearer Fonts on OS X (Emacs version < 26)

## Problem

Fonts are blurry in the GUI version of Emacs on Mac OS X.


## Solution

There are a few ways to clear up blurry fonts in OS X. So far, the best results
have been to use lighter weight fonts. Here is an example using [Source Code
Pro]:

```lisp
;; lightweight font
(set-face-attribute 'default nil :family "Source Code Pro" :weight 'light)
(set-face-attribute 'default nil :height 140)


(defun faces-bold-to-semibold ()
  "Switch bold font to semibold."
  (mapc
   (lambda (face)
     (when (eq (face-attribute face :weight) 'bold)
       (set-face-attribute face nil :weight 'normal)))
   (face-list)))

;; switch org and markdown mose to repalce bold faces with semibold
(when (eq system-type 'darwin)
  (mapc (lambda (mode)
          (eval-after-load mode #'faces-bold-to-semibold))
        '("markdown-mode" "org")))
```

[Source Code Pro]: https://github.com/adobe-fonts/source-code-pro
