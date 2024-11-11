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

;; diff-hl configuration
(use-package diff-hl :demand t :ensure t)
(global-diff-hl-mode)

(provide 'vc)
;;; vc.el ends here
