;;; neotree.el --- Configuration for Neotree
;;; Commentary:
;; This file configures Neotree, a file and directory visualization tool
;; in Emacs.  It allows for easy navigation of your project's file structure.

;;; Code:

(use-package neotree
  :ensure t
  :bind (("M-\\" . neotree-toggle))
  :config
  (setq neo-window-width 35)
  (setq neo-theme 'icons)
  (setq neo-window-fixed-size nil)
  (setq delete-by-moving-to-trash t))

;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq treemacs-file-event-delay 500)
;;   (treemacs-follow-mode 1)
;;   (treemacs-fringe-indicator-mode t)
;;   (treemacs-filewatch-mode t)
;;   (add-hook 'projectile-after-switch-project-hook
;;             (lambda ()
;;               (treemacs-select-window)
;;               (treemacs-refresh))))

;; (global-set-key (kbd "M-\\") 'treemacs)

;; (use-package treemacs-nerd-icons
;;   :config
;;   (treemacs-load-theme "nerd-icons"))

;; (use-package treemacs-projectile
;;   :after (treemacs projectile)
;;   :ensure t
;;   :config
;;   (treemacs-project-follow-mode 1)) ;; Ativa o modo de acompanhamento do projeto

;; (use-package treemacs-icons-dired
;;   :ensure t
;;   :after (treemacs dired)
;;   :config
;;   (treemacs-icons-dired-mode 1))  ;; Ativa o modo


(provide 'neotree)
;;; neotree.el ends here
