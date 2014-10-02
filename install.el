;;; install.el --- Emacs package install script
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar my-packages
  '(org markdown-mode inf-ruby yaml-mode yasnippet rainbow-mode js-comint web-mode multi-term erlang)
  "A list of packages to install.")

(package-refresh-contents)    ; updates packages lists

(dolist (p my-packages)
  (condition-case nil
      (package-install p)
    (error
     (message "WARNING: skipping %s" p))) )
