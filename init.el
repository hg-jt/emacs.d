;;; init.el --- Emacs configuration

;; UI
(unless (eq system-type 'darwin)
  (if (fboundp 'menu-bar-mode) (menu-bar-mode 0)))     ; show menu-bar on OS-X only
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
(if (fboundp 'mouse-wheel-mode) (mouse-wheel-mode 1))

(line-number-mode 1)                                   ; show line numbers in mode bar
(column-number-mode 1)                                 ; show column numbers in mode bar
(show-paren-mode 1)                                    ; highlight matching parens
(global-font-lock-mode 1)                              ; enable syntax highlighting
(blink-cursor-mode 0)                                  ; chill the cursor out
(delete-selection-mode 1)                              ; replace selection with typed text

;; default settings
(setq-default initial-scratch-message nil              ; disable scratch buffer cruft
              indent-tabs-mode nil                     ; use spaces instead of tabs
              indicate-empty-lines t)                  ; indicates an empty line in the fringe

;; general configuration
(setq inhibit-startup-message t                        ; disable startup messages
      default-tab-width 4                              ; set tab width to 4
      make-backup-files nil                            ; stop creating backup~ files
      compilation-scroll-output t)                     ; scroll the compilation buffer


;; use utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


;; configure global key bindings
(define-key global-map [f5] 'compile)                  ; compile
(define-key global-map [f7] 'kill-compilation)         ; close compile frame


;; configure OS X specific keybindings
(when (eq system-type 'darwin)
  ;; fix home and end keys from a pc keyboard's 3x2 cluster
  (define-key global-map [home] 'beginning-of-line)
  (define-key global-map [end] 'end-of-line))


;; configure load-path
(defun add-subdirs-to-load-path (dir)
  "Adds all sub-directories of dir to he load path.

ex: (add-to-list 'load-path \"~/.emacs.d/site-lisp\")
    (add-subdirs-to-load-path \"~/.emacs.d/site-lisp\")"
  (let ((default-directory (concat dir "/")))
    (normal-top-level-add-subdirs-to-load-path)))

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-subdirs-to-load-path "~/.emacs.d/site-lisp")


;; configure sh-mode
(setq-default sh-basic-offset 2
              sh-indentation 2)


;; configure cc-mode
(eval-after-load "cc-mode"
  '(progn
     ;(which-function-mode)
     (define-key c-mode-map (kbd "C-c o") 'ff-find-other-file)))


;; configure org-mode
(eval-after-load "org"
  '(progn
     ;; configure org behavior
     (setq org-startup-indented nil                    ; not sure if this is working
           org-hide-leading-stars t                    ; hiding excess stars for readability
           org-odd-levels-only t                       ; skip levels for readability
           org-log-done t                              ; adds a timestamp to completed tasks
           org-alphabetical-lists t                    ; enable single character alpha bullets
           org-src-fontify-natively t)                 ; makes code blocks pretty

     ;; configure org export
     (setq org-export-ascii-underline '(?\- ?\= ?\~ ?\# ?^ ?\$)  ; configure ascii export underlines
           org-export-latex-listings t)                          ; use the listings package in LaTeX export

     ;; configure capture templates
     (setq org-capture-templates
           '(("j" "Journal" entry (file+datetree "~/notes/journal.org") "* %?")))

     ;; define TODO task workflow
     (setq org-todo-keywords
           '((sequence "TODO" "IN PROGRESS" "|" "DONE")))

     ;; configure keybindings
     (define-key global-map (kbd "C-c c") 'org-capture)     ; capture
     ;; configure org export (ox)
     ;(require 'ox-beamer)

     (defun org-summary-todo (n-done n-not-done)
       "Switch entry to DONE when all subentries are done, to TODO otherwise."
       (let (org-log-done org-log-states)   ; turn off logging
         (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

     (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
     (add-hook 'org-mode-hook
               (lambda ()
                 ;; configure editing preferences
                 (setq fill-column 100
                       sentence-end-double-space nil)
                 (turn-on-auto-fill))
               t)))


;; configure ruby-mode
(when (locate-library "ruby-mode")
  (setq-default ruby-indent-level 2)
  (setq auto-mode-alist (append '(("\\.gemspec" . ruby-mode)
                                  ("\\.irbrc" . ruby-mode)
                                  ("\\.builder" . ruby-mode)
                                  ("Rakefile" . ruby-mode)
                                  ("Gemfile" . ruby-mode)
                                  ("Capfile" . ruby-mode))
                                auto-mode-alist)) )


;; configure python.el
(when (locate-library "python")
  (setq auto-mode-alist (append '(("\\.pythonrc\\'" . python-mode)) auto-mode-alist)))


;; configure js-mode
(setq-default js-indent-level 2)


;; configure latex-mode
(add-to-list 'auto-mode-alist '("\\.latex\\'" . latex-mode))


;; configure web-mode
(add-hook 'after-init-hook
          (lambda ()
            (when (locate-library "web-mode")
              (setq-default web-mode-markup-indent-offset 2)
              (setq auto-mode-alist (append '(("\\.html?\\'" . web-mode)
                                              ("\\.erb\\'" . web-mode)
                                              ("\\.rhtml\\'" . web-mode)
                                              ("\\.j2\\'" . web-mode)
                                              ("\\.jsp\\'" . web-mode))
                                            auto-mode-alist))) ))


;; configure inferior-js-mode
(add-hook 'after-init-hook
          (lambda ()
            (when (locate-library "js-comint")
              (defun js-send-last-defun ()
                "Send the previous defun to the inferior Javascript process."
                (interactive)
                (js-send-region (save-excursion (beginning-of-defun) (point)) (point)))

              (defun inferior-js-keybindings ()
                (define-key js-mode-map (kbd "C-x C-e") 'js-send-last-sexp)
                (define-key js-mode-map (kbd "C-x C-E") 'js-send-last-defun)
                (define-key js-mode-map (kbd "C-M-x") 'js-send-last-sexp-and-go)
                (define-key js-mode-map (kbd "C-c b") 'js-send-buffer)
                (define-key js-mode-map (kbd "C-c C-b") 'js-send-buffer-and-go)
                (define-key js-mode-map (kbd "C-c l") 'js-load-file-and-go))

              (add-hook 'inferior-js-mode-hook 'inferior-js-keybindings) )))


;; configure markdown-mode
(add-hook 'markdown-mode-hook
          (lambda ()
            (set-fill-column 80)
            (turn-on-auto-fill)))


;; configure scala-mode
(add-hook 'scala-mode-hook
          (lambda ()
            (when (>= emacs-major-version 24)
              (electric-pair-mode 1))))
