;;; install.el --- Emacs package install script

;; Commentary:
;;
;; This file iterates through a list of packages and installs them.
;;
;; The following list of packages may be interesting, but are not currently
;; included:
;;
;;    erlang, multi-term, rainbow-delimiters, paredit
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(defvar my-packages
  '(
    org
    markdown-mode
    inf-ruby
    yaml-mode
    yasnippet
    rainbow-mode
    js-comint
    auto-complete
    web-mode
    magit
    scala-mode2
    clojure-mode
    clojure-mode-extra-font-locking
    cider)
  "A list of packages to install.")

(package-refresh-contents)    ; updates package lists

(dolist (p my-packages)
  (condition-case nil
      (package-install p)
    (error
     (message "WARNING: skipping %s" p))) )
