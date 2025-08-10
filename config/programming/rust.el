;;; rust.el --- Rust Development Setup

;;; Code:

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save nil)
  :hook ((rust-mode . lsp)
         (rust-mode . (lambda ()
                        (setq-local company-backends
                                    '(company-capf company-dabbrev))))))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(provide 'rust)

;;; rust.el ends here
