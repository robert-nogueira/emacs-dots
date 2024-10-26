;;; vc.el --- Configuration for Version Control
;;; Commentary:
;; This file configures Magit and diff-hl for version control in Emacs.
;; Magit provides an interface for Git, while diff-hl highlights uncommitted
;; changes in the fringe.

;;; Code:

;; Magit configuration
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))  ; Bind `C-x g` to open Magit status
  :config
)

;; diff-hl configuration
(use-package diff-hl
  :ensure t
  :hook (prog-mode . diff-hl-mode)  ; Enable diff-hl in programming modes
  :config
  (setq diff-hl-draw-borders nil)    ; Set to `nil` for cleaner display
)

(provide 'vc)
;;; vc.el ends here
