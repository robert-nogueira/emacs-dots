;;; python.el --- Python Development Setup

;;; Commentary:
;; This file configures Emacs packages for Python development,
;; including LSP with Pyright, Poetry for project management,
;; and TOML mode.

;;; Code:

(use-package poetry
  :straight t
  :ensure t
  :config
  (poetry-tracking-mode))

(use-package python
  :mode "python-mode"
  :hook
  (python-mode . (lambda ()
                   (when (poetry-venv-exist-p)
                     (message "poetry-venv-exist-p: %s"
                              (symbol-value 'poetry-venv-exist-p))
                     (lsp-deferred))))
  :config
  (setq python-indent-guess-indent-offset 4))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-disabled-clients '(ruff semgrep-ls))
  :hook ((python-mode . lsp))
  :config
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-enable-snippet nil))

(add-hook 'python-mode-hook
          (lambda ()
            (setq-local company-backends
                        '(company-capf company-dabbrev))))

(add-hook 'python-mode-hook 'display-line-numbers-mode)
(add-hook 'python-mode-hook #'font-lock-mode)

(use-package lsp-pyright
  :ensure t
  :hook
  ((python-mode . (lambda ()
                    (require 'lsp-pyright)
                    (setq lsp-pyright-venv-path
                          (string-trim
                           (shell-command-to-string
                            "poetry env info -p")))
                    (lsp-deferred)))))

(advice-add 'lsp--info :around (lambda (&rest _) nil))

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'top
        lsp-ui-doc-side 'right
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-use-childframe t
        lsp-ui-peek-enable t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-check-mode 'project
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-doc-delay 0
        lsp-ui-doc-max-height 70
        lsp-ui-doc-border "#cba6f7")

  (define-key lsp-ui-mode-map
    [remap xref-find-definitions]
    #'lsp-ui-peek-find-definitions)

  (define-key lsp-ui-mode-map
    [remap xref-find-references]
    #'lsp-ui-peek-find-references)

  (global-set-key (kbd "C-c C-d") 'lsp-ui-doc-show))

(use-package toml-mode
  :ensure t)

(use-package numpydoc
  :ensure t
  :bind
  ("C-c d" . numpydoc-generate))

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c r") #'lsp-rename))

(provide 'python)

;;; python.el ends here
