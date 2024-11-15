;;; treemacs.el --- Configuration for Treemacs
;;; Commentary:
;; This file configures Treemacs, a file and directory visualization tool
;; in Emacs.  It allows for easy navigation of your project's file structure.

;;; Code:

(use-package treemacs
  :ensure t
  :bind (("M-\\" . treemacs))
  :config
  (setq treemacs-hide-gitignored-files-mode t)
  (setq treemacs-project-follow-cleanup t)
  (setq treemacs-width 35)
  (setq treemacs-width-is-initially-locked nil)
  (setq delete-by-moving-to-trash t)
  (setq treemacs-collapse-dirs 3)
  (setq treemacs-display-in-side-window t)
  (setq treemacs-is-never-other-window t)
  (setq treemacs-indentation 2)
  (setq treemacs-indentation-string " ")
  (setq treemacs-filewatch-mode t)
  (setq treemacs-git-mode 'deferred)
  (setq treemacs-file-event-delay 0)
  (add-hook 'treemacs-mode-hook #'treemacs-project-follow-mode))


(use-package treemacs-nerd-icons
  :after nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(custom-set-faces
  '(treemacs-git-modified-face ((t (:foreground "#cba6f7" :weight bold))))
  '(treemacs-git-added-face ((t (:foreground "#94e2d5" :weight bold))))
  '(treemacs-git-untracked-face ((t (:foreground "#f38ba8" :weight bold)))))

(provide 'treemacs)
;;; treemacs.el ends here
