;;; install.el --- Emacs package install script

;; Commentary:
;;
;; This file iterates through a list of packages and installs them.
;;
;; The following list of packages may be interesting, but are not currently
;; included:
;;
;;    erlang, multi-term, rainbow-delimiters, paredit, exec-path-from-shell
(require 'package)
(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ;("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable". "https://stable.melpa.org/packages/")))

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
    cider
    dockerfile-mode
    less-css-mode
    jade-mode)
  "A list of packages to install.")

(package-refresh-contents)    ; updates package lists

(dolist (p my-packages)
  (condition-case nil
      (package-install p)
    (error
     (message "WARNING: skipping %s" p))) )
