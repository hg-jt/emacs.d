#!/usr/bin/env emacs --script
;;; install.el --- Emacs package install script

;; Commentary:
;;
;; This file iterates through a list of packages and installs them.
;;
;; The following list of packges were interesting, but are no longer part of
;; this script:
;;
;;   erlang, mult-term, rainbow-delimiters, paredit, exec-path-from-shell,
;;   jade-mode, less-css-mode, salt-mode, auto-complete

;; configure load-path for non-interactive use
(if noninteractive
    (add-to-list 'load-path "~/.emacs.d/site-lisp"))


;; configure reasonable certificate trust for Emacs on OS X.
(if (and (eq system-type 'darwin)
         (file-exists-p "/usr/local/etc/openssl/cert.pem"))
    (eval-after-load "gnutls"
      (lambda ()
        ;; a brew installed openssl will include the cert.pem file in
        ;; /usr/local/etc/openssl
        (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem"))))


(require 'package)
(package-initialize t)

;; configure ELPA repositories
(if (< emacs-major-version 24)
    ;; package repositories for older emacsen (note the lack of https)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  ;; package repositories for modern emacsen
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ;("marmalade" . "https://marmalade-repo.org/packages/")
                           ;("melpa-stable" . "https://stable.melpa.org/packages/")
                           ("melpa" . "https://melpa.org/packages/"))))


(defvar my-packages
  '(;; misc. languages
    dockerfile-mode
    go-mode
    ini-mode
    markdown-mode
    nginx-mode
    rust-mode
    scala-mode
    web-mode
    yaml-mode

    ;; polymodes
    polymode
    poly-erb
    poly-markdown
    poly-rst

    ;; lsp
    lsp-mode
    lsp-pyright

    ;; org-mode
    ob-mermaid
    org-plus-contrib
    ox-reveal
    htmlize

    ;; inferior modes
    inf-ruby
    js-comint

    ;; vcs
    magit

    ;; extras
    company
    docker-tramp
    flycheck
    keycast
    package-lint
    rainbow-mode
    restclient
    skeletor
    uuid
    yasnippet

    ;; themes
    dracula-theme
    gruvbox-theme)
  "A list of packages to install.")


;; update package lists
(package-refresh-contents)


;; install packges
(dolist (p my-packages)
  (condition-case nil
      (package-install p)
    (error
     (message "WARNING: skipping %s" p))) )
