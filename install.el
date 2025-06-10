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
;;   jade-mode, less-css-mode, salt-mode, auto-complete, scala-mode,
;;   docker-tramp, dockerfile-mode, go-mode, lsp-mode, lsp-pyright

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
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("melpa-stable" . "https://stable.melpa.org/packages/"))))


;; tree sitter languages
;; see https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.23.3")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml") ))


(defvar my-packages
  '(;; misc. languages
    groovy-mode
    ini-mode
    markdown-mode
    nginx-mode
    php-mode
    rust-mode
    web-mode
    yaml-mode

    ;; polymodes
    polymode
    poly-erb
    poly-markdown
    poly-rst

    ;; org-mode
    org
    org-contrib
    ob-mermaid
    ox-reveal
    htmlize

    ;; inferior modes
    inf-ruby
    js-comint

    ;; vcs
    magit

    ;; extras
    company
    flycheck
    flymake-ruff
    git-modes
    keycast
    package-lint
    pip-requirements
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


;; install language grammers
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (mapc #'treesit-install-language-grammar
        (mapcar #'car treesit-language-source-alist)))
