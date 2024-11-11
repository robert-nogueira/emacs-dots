;;; settings.el --- General Emacs settings
;;; Commentary:
;; General configurations to adjust the behavior of Emacs.

;;; Code:

(setq-default electric-indent-inhibit t)  ;; Disable electric indentation
(setq ring-bell-function 'ignore)          ;; Disable the bell sound
(setq select-enable-clipboard t)            ;; Enable clipboard selection
(setq-default cursor-type 'bar)             ;; Set cursor type to bar
(setq scroll-step 1                         ;; Set scrolling step
      scroll-conservatively 10000)          ;; Scroll conservatively
(setq inhibit-startup-message t)            ;; Disable the startup message
(setq ring-bell-function 'ignore)

;; no backup files
(setq make-backup-files nil)

(setq backup-directory-alist '((".*" . "~/.saves")))

;; no lock files
(setq create-lockfiles nil)

;; automatically update the file if there are any changes
(global-auto-revert-mode t)

(setq-default truncate-lines t)

;;; settings.el ends here
(provide 'settings)
