;;; discord.el --- Elcord Configuration for Discord Rich Presence

;;; Code:

(use-package elcord
  :straight (elcord :type git :host github :repo "robert-nogueira/elcord-enhanced")
  :defer nil
  :config
  (setq elcord-switch-icons t)
  (setq elcord-refresh-rate 5)
  (setq elcord-buttons '((:label "📂 GitHub"
				 :url "https://github.com/robert-nogueira")))
  (elcord-mode))


	(provide 'discord)
;;; discord.el ends here
