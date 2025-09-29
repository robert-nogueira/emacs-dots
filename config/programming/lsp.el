;;; lsp-config.el --- LSP and related UI configuration

;;; Code:

(use-package lsp-mode
  :ensure t
  :defer nil
  :config
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-enable-snippet nil
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-completion-add-call-parenthesis t
        lsp-rust-analyzer-completion-add-call-argument-snippets t)
  :hook ((rust-mode . lsp)))

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

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c r") #'lsp-rename))

(advice-add 'lsp--info :around (lambda (&rest _) nil))

(provide 'lsp-config)

;;; lsp-config.el ends here
