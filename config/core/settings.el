;;; settings.el --- General Emacs settings
;;; Commentary:
;; General configurations to adjust the behavior of Emacs.

;;; Code:

(setq-default electric-indent-inhibit t)
(setq ring-bell-function 'ignore)
(setq select-enable-clipboard t)
(setq-default cursor-type 'bar)
(setq scroll-step 1
      scroll-conservatively 10000)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq save-interprogram-paste-before-kill nil)
;; (setq inhibit-startup-echo-area-message t)

(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq backup-directory-alist '((".*" . "~/.saves")))
(global-auto-revert-mode t)
(setq-default truncate-lines t)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(setq redisplay-dont-pause t)
(setq vc-follow-symlinks t)

(desktop-save-mode 1)
(unless (file-directory-p "~/.emacs.d/desktop/")
  (make-directory "~/.emacs.d/desktop/" t))
(setq desktop-path '("~/.emacs.d/desktop/"))
(setq desktop-dirname "~/.emacs.d/desktop/")
(setq desktop-base-file-name "emacs-desktop")

;;; settings.el ends here
(provide 'settings)
