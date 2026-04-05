;;; early-init.el --- Early macs configuration.

;;; Code:

;; increase gc-threshold for startup
(setq gc-cons-threshold (* 10 1000 1000)  ;; 10MB
      gc-cons-percentage 0.6)

;; set elevated gc threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 4 1000 1000)
                  gc-cons-percentage 0.1)))
;;; early-init.el ends here
