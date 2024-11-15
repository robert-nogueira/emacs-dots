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
  :bind (("C-x g" . magit-status))  ; Bind `C-x g` to open Magit status
  :config
)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t))

(setq git-gutter:added-sign "✨")
(setq git-gutter:modified-sign "♻️")
(setq git-gutter:deleted-sign "🔥")

(provide 'vc)
;;; vc.el ends here
