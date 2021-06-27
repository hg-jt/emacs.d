;;; init.el --- Emacs configuration

;; UI
(unless (and (eq system-type 'darwin) (window-system))
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
              indicate-empty-lines t                   ; indicates an empty line in the fringe
              ;display-line-numbers t                   ; show line numbers with absolute numbering
              major-mode (lambda ()                    ; use auto-modes for non-file buffers
                           (if buffer-file-name
                               'fundamental-mode
                             (let ((buffer-file-name (buffer-name)))
                               (set-auto-mode)))) )


;; general configuration
(setq inhibit-startup-message t                        ; disable startup messages
      default-tab-width 4                              ; set tab width to 4
      make-backup-files nil                            ; stop creating backup~ files
      compilation-scroll-output t                      ; scroll the compilation buffer
      gc-cons-threshold 10000000                       ; increase threshold for running gc
      auto-window-vscroll nil                          ; potentially speed up line navigation
      custom-file "~/.emacs.d/custom.el")              ; isolate customizations

;; use utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


;; configure global key bindings
(define-key global-map [f5] 'compile)                  ; compile
(define-key global-map [f7] 'kill-compilation)         ; close compile frame
(define-key global-map (kbd "C-c c") 'org-capture)     ; org-capture


;; configure OS X specific keybindings
(when (eq system-type 'darwin)
  ;; works around terminal issues w/properly sending meta in OSX
  (define-key global-map [f8] 'query-replace-regexp)

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

(let ((local-site-lisp-dir "~/.emacs.d/site-lisp"))
  (when (file-directory-p local-site-lisp-dir)
    (add-to-list 'load-path local-site-lisp-dir)
    (add-subdirs-to-load-path local-site-lisp-dir)))


;; initialize ELPA for older emacsen
(when (and (< emacs-major-version 24) (locate-library "package"))
  (require 'package)
  (package-initialize))


;; add support for with-eval-after-load macro for older emacsen
(when (not (fboundp 'with-eval-after-load))
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file (lambda () ,@body))))


;; configure sh-mode
(setq-default sh-basic-offset 2
              sh-indentation 2)


;; configure cc-mode
(with-eval-after-load "cc-mode"
  ;(which-function-mode)
  (define-key c-mode-map (kbd "C-c o") 'ff-find-other-file))


;; configure org-mode
(with-eval-after-load "org"
  ;; configure org behavior
  (setq org-hide-leading-stars t                    ; hiding excess stars for readability
        org-odd-levels-only t                       ; skip levels for readability
        org-log-done t                              ; adds a timestamp to completed tasks
        org-alphabetical-lists t                    ; enable single character alpha bullets
        org-src-fontify-natively t                  ; makes code blocks pretty

        ;; org export
        org-export-ascii-underline '(?\- ?\= ?\~ ?\# ?^ ?\$)  ; configure ascii export underlines
        org-latex-listings t                                  ; use the listings package in LaTeX export

        ;; TODO task workflow
        org-todo-keywords '((sequence "TODO(t)" "IN PROGRESS(p)" "|" "DONE(d)" "OBE(o)"))

        ;; org-agenda
        org-agenda-window-setup 'current-window)


  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  (defun org-editing-preferences ()
    "Set basic buffer level editing preferences."
    (setq fill-column 80
          sentence-end-double-space nil)
    (turn-on-auto-fill))

  (add-hook 'org-mode-hook 'org-editing-preferences) )


;; configure ruby-mode
(when (locate-library "ruby-mode")
  (setq-default ruby-indent-level 2)
  (setq auto-mode-alist (append '(("\\.gemspec\\'" . ruby-mode)
                                  ("\\.irbrc\\'" . ruby-mode)
                                  ("\\.builder\\'" . ruby-mode)
                                  ("\\(?:Rake\\|Cap\\|Vagrant\\)file\\'" . ruby-mode))
                                auto-mode-alist)) )


;; configure python.el
(when (locate-library "python")
  (setq auto-mode-alist (append '(("\\.pythonrc\\'" . python-mode)) auto-mode-alist))

  ;; declare 'project-venv-name' as safe variable when defined
  ;; as a string in .dir-locals.el or as a file local variable.
  ;(put 'project-venv-name 'safe-local-variable #'stringp)
  (put 'python-shell-interpreter 'safe-local-variable #'stringp)

  ;; initialize pylint via virtualenv and flycheck
  ;; @see https://github.com/porterjamesj/virtualenvwrapper.el
  (add-hook 'after-init-hook
            (lambda ()
              (when (locate-library "flycheck")
                (add-hook 'python-mode-hook (lambda ()
                                              ;; attempt to use virtualenvwrapper.el
                                              (hack-local-variables)
                                              (when (and
                                                     (locate-library "virtualenvwrapper")
                                                     (boundp 'project-venv-name))
                                                (venv-workon project-venv-name)))) ))) )


;; configure js-mode
(setq-default js-indent-level 2)


;; configure latex-mode
(add-to-list 'auto-mode-alist '("\\.latex\\'" . latex-mode))


;; configure packages most likely installed through the package manager
(add-hook 'after-init-hook
          (lambda ()
            ;; configure markdown-mode
            (when (locate-library "markdown-mode")
              (add-hook 'markdown-mode-hook
                        (lambda ()
                          (set-fill-column 80) ))
                          ;(turn-on-auto-fill)))
              (add-to-list 'auto-mode-alist '("\\*md\\*\\'" . markdown-mode)))


            ;; configure restclient-mode
            (when (locate-library "restclient")
              (add-to-list 'auto-mode-alist '("\\*web\\*\\'" . restclient-mode)))


            ;; configure web-mode
            (when (locate-library "web-mode")
              (setq-default web-mode-markup-indent-offset 2)
              (setq auto-mode-alist (append '(("\\.r?html?\\'" . web-mode)
                                              ("\\.erb\\'" . web-mode)
                                              ("\\.j2\\'" . web-mode)
                                              ("\\.jsp\\'" . web-mode)
                                              ("\\.jinja\\'" . web-mode))
                                            auto-mode-alist)))


            ;; configure inferior-js-mode
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

              (add-hook 'inferior-js-mode-hook 'inferior-js-keybindings))


            ;; configure magit
            (when (locate-library "magit")
              (global-set-key (kbd "C-x g") 'magit-status)

              (with-eval-after-load "magit"
                (defun magit-kill-buffers ()
                  "Restore window configuration and kill all Magit buffers.

See http://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/"
                  (interactive)
                  (let ((buffers (magit-mode-get-buffers)))
                    (magit-restore-window-configuration)
                    (mapc #'kill-buffer buffers)))
                (define-key magit-status-mode-map (kbd "q") #'magit-kill-buffers)))


            ;; configure scala-mode
            (when (and (locate-library "scala-mode") (>= emacs-major-version 24))
              (add-hook 'scala-mode-hook 'electric-pair-mode))


            ;; configure polymodes
            (when (locate-library "polymode")
              ;; poly-erb
              (when (locate-library "poly-erb")
                (setq auto-mode-alist (append '(("\\.js.erb\\'" . poly-js+erb-mode)
                                                ("\\.coffee.erb\\'" . poly-coffee+erb-mode)
                                                ("\\.html.erb\\'$" . poly-html+erb-mode))
                                              auto-mode-alist)))) ))
