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
  (with-eval-after-load 'treemacs
    (defun treemacs-ignore-gitignore (file _)
      (string= file "__pycache__"))
    (push #'treemacs-ignore-gitignore treemacs-ignored-file-predicates))
(add-hook 'treemacs-mode-hook #'treemacs-project-follow-mode))

(defvar my-themes '("doom-moonlight"))


(use-package treemacs-nerd-icons
  :after nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(with-eval-after-load 'treemacs-nerd-icons
  (set-face-foreground 'treemacs-git-modified-face "#94e2d5")
  (set-face-foreground 'treemacs-git-added-face "#cba6f7")
  (set-face-foreground 'treemacs-git-untracked-face "#f38ba8")
  (when (or (not custom-enabled-themes)
              (cl-find-if (lambda (theme) (member theme my-themes))
                          (mapcar #'symbol-name custom-enabled-themes)))
  (set-face-foreground 'treemacs-directory-face "#89b4fa")
  (set-face-foreground 'treemacs-nerd-icons-file-face "#89b4fa")))


(provide 'treemacs)
;;; treemacs.el ends here
