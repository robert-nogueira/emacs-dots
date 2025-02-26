;;; discord.el --- Elcord Configuration for Discord Rich Presence
;;; Commentary:

;; This file configures Elcord, enabling Discord Rich Presence
;; integration for Emacs.

;;; Code:

(use-package elcord
  :ensure t
  :config
  (elcord-mode)
  (setq elcord-idle-timer 600)
  (setq elcord-idle-message "Tomando uma brejinha..."))

(provide 'elcord)
;;; discord.el ends here
