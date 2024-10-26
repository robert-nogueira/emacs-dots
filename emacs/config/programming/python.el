;;; python.el --- Python Development Setup
;;; Commentary:

;; This file configures Emacs packages for Python development,
;; including Jedi for auto-completion, Poetry for project management,
;; and TOML mode.

;;; Code:

;; (use-package jedi
;;   :ensure t
;;   :init
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (add-hook 'python-mode-hook 'jedi:ac-setup)
;;   :config
;;   (setq jedi:use-shortcuts t
;;         jedi:complete-on-dot t))

(use-package poetry
  :ensure t
  :hook (python-mode . poetry-tracking-mode))

(add-hook 'python-mode-hook 'company-mode)
(setq python-indent-guess-indent-offset nil)  ;; Desabilitar a adivinhação de indentação

(use-package toml-mode
  :ensure t)

(provide 'python)
;;; python.el ends here
