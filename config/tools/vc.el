;;; vc.el --- Configuration for Version Control
;;; Commentary:
;; This file configures Magit and diff-hl for version control in Emacs.
;; Magit provides an interface for Git, while diff-hl highlights uncommitted
;; changes in the fringe.

;;; Code:

;; Magit configuration
(use-package magit
  :ensure t
  :defer nil
  :bind (("C-x g" . magit-status)))

(global-set-key (kbd "C-c g") 'magit-diff-buffer-file)

;; (use-package git-gutter
;;   :ensure t
;;   :config
;;   (global-git-gutter-mode +1))

;; (setq git-gutter:added-sign "✨")
;; (setq git-gutter:modified-sign "♻️")
;; (setq git-gutter:deleted-sign "🔥")
;; (add-hook 'python-mode-hook 'git-gutter-mode)

(use-package diff-hl
  :ensure t
  :after magit
  :defer nil
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  (diff-hl-dired-mode 1)
  (diff-hl-margin-mode 1)
  (diff-hl-amend-mode 1))

(provide 'vc)
;;; vc.el ends here
