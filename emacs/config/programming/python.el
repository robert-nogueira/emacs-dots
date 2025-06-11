;;; python.el --- Python Development Setup

;;; Code:

(use-package poetry
  :straight t
  :ensure t
  :config (poetry-tracking-mode))

(use-package python
  :mode "python-mode"
  :config (setq python-indent-guess-indent-offset 4))


(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			 (when (poetry-venv-exist-p)
			   (setq lsp-pyright-venv-path (string-trim (shell-command-to-string "poetry env info -p")))
			   (lsp-deferred)))))

(add-hook 'python-mode-hook #'display-line-numbers-mode)

(use-package toml-mode
  :ensure t)

(use-package numpydoc
  :ensure t
  :bind ("C-c d" . numpydoc-generate))

(provide 'python)

;;; python.el ends here
