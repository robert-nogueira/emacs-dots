;;; python.el --- Python Development Setup
;;; Commentary:

;; This file configures Emacs packages for Python development,
;; including Jedi for auto-completion, Poetry for project management,
;; and TOML mode.

;;; Code:
(use-package poetry
  :straight t
  :demand 
  :config
  (poetry-tracking-mode)
  )

(use-package python
  :mode "python-mode"
  :after (poetry)
  :hook
  (python-mode . (lambda () 
                   (when (poetry-venv-exist-p)
                     (lsp-deferred))
  :config
  (setq python-indent-guess-indent-offset nil))))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-disabled-clients '(ruff))
  :hook ((python-mode . lsp))
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-diagnostic-package :none)
  (setq lsp-enable-snippet nil))

(add-hook 'python-mode-hook (lambda () (setqsetq-local company-backends '(company-capf company-dabbrev))))

(use-package lsp-pyright
  :ensure t
  :hook
  ((python-mode . (lambda ()
                    (require 'lsp-pyright)
                    (lsp-deferred)))))

(setq lsp-auto-guess-root t)


(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'top
	lsp-ui-doc-show-with-cursor t
        lsp-ui-peek-enable t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-check-mode 'project
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
	lsp-ui-doc-delay 0.5
        lsp-ui-doc-max-width 80
        lsp-ui-doc-max-height 20)
 (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
 (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (global-set-key (kbd "C-c C-d") 'lsp-ui-doc-show))

(setq lsp-ui-doc-enable nil)

(use-package toml-mode
  :ensure t)

(use-package numpydoc
  :ensure t
  :bind
  ("C-c d" . numpydoc-generate))


(provide 'python)
;;; python.el ends here
