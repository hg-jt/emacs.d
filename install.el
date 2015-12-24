;;; install.el --- Emacs package install script

;; Commentary:
;;
;; This file iterates through a list of packages and installs them.
;;
;; The following list of packges were interesting, but are no longer part of
;; this script:
;;
;;   erlang, mult-term, rainbow-delimiters, paredit, exec-path-from-shell

;; configure load-path for non-interactive use
(if noninteractive
    (add-to-list 'load-path "~/.emacs.d/site-lisp"))


(require 'package)
(package-initialize t)

;; configure ELPA repositories
(if (< emacs-major-version 24)
    ;; package repositories for older emacsen
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  ;; package repositories for modern emacsen
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("melpa-stable". "https://stable.melpa.org/packages/"))) )


;; pin stable versions of cider/clojure-mode when possible
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((clojure-mode . "melpa-stable")
          (clojure-mode-extra-font-locking . "melpa-stable")
          (cider . "melpa-stable"))) )


(defvar my-packages
  '(;; misc. languages
    dockerfile-mode
    jade-mode
    less-css-mode
    markdown-mode
    scala-mode2
    web-mode
    yaml-mode

    ;; clojure
    clojure-mode
    clojure-mode-extra-font-locking
    cider

    ;; org-mode
    org
    ox-reveal

    ;; ext. tools
    inf-ruby
    js-comint
    magit

    ;; extras
    auto-complete
    rainbow-mode
    restclient
    yasnippet)
  "A list of packages to install.")


;; update package lists
(package-refresh-contents)


;; install packges
(dolist (p my-packages)
  (condition-case nil
      (package-install p)
    (error
     (message "WARNING: skipping %s" p))) )
