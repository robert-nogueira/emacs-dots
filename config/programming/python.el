;;; python.el --- Python Development Setup

;;; Code:

(require 'functions)

(use-package poetry
  :straight t
  :ensure t
  :config (poetry-tracking-mode))

(use-package python
  :mode "python-mode"
  :config (setq python-indent-guess-indent-offset 4))

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-M-l") #'my/ruff-format-buffer)
            (add-hook 'before-save-hook #'my/ruff-format-buffer nil t)))

(use-package lsp-pyright
  :ensure t
  :init
  (setq lsp-pyright-diagnostic-mode "workspace")
  :hook (python-mode . (lambda ()
			 (when (locate-dominating-file default-directory "pyproject.toml")
			   (let ((venv (string-trim (shell-command-to-string "poetry env info -p"))))
			     (setq-local lsp-pyright-venv-path
					 (and (not (string-empty-p venv)) (file-directory-p venv) venv)))
			   (lsp-deferred))
			 )))

;; (add-hook 'python-mode-hook #'display-line-numbers-mode)

(use-package toml-mode
  :ensure t)

;; (use-package numpydoc
;;   :ensure t
;;   :bind ("C-c d" . numpydoc-generate))

(provide 'python)

;;; python.el ends here
