;;; treemacs.el --- Configuration for Treemacs
;;; Commentary:
;; This file configures Treemacs, a file and directory visualization tool
;; in Emacs.  It allows for easy navigation of your project's file structure.

;;; Code:

(use-package treemacs
  :ensure t
  :defer nil
  :bind (("M-\\" . treemacs))
  :config
  (setq treemacs-hide-gitignored-files-mode t)
  (setq treemacs-project-follow-cleanup t)
  (setq treemacs-width 45)
  (setq treemacs-width-is-initially-locked nil)
  (setq delete-by-moving-to-trash t)
  (setq treemacs-collapse-dirs 3)
  (setq treemacs-display-in-side-window t)
  (setq treemacs-is-never-other-window t)
  (setq treemacs-indentation 2)
  (setq treemacs-indentation-string " ")
  (setq treemacs-filewatch-mode t)
  (setq treemacs-git-mode 'deferred)
  (setq treemacs-text-scale 2)
  (setq treemacs-move-files-by-mouse-dragging nil)
  (setq treemacs-move-forward-on-expand t)
  (setq treemacs-pulse-on-success t)
  (setq treemacs-file-event-delay 0)
  (setq treemacs-deferred-git-apply-delay 0)
  (setq treemacs-git-commit-diff-mode 1)

  (defun treemacs-ignore-gitignore (file _)
    (string= file "__pycache__"))
  (push #'treemacs-ignore-gitignore treemacs-ignored-file-predicates))

(add-hook 'treemacs-mode-hook #'treemacs-project-follow-mode)

(use-package treemacs-nerd-icons
  :after nerd-icons
  :defer nil
  :config
  (treemacs-load-theme "nerd-icons"))

(provide 'treemacs)
;;; treemacs.el ends here
