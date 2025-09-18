;;; hooks-lsp-terraform.el --- lsp-terraform hook configuration

;;; Commentary:
;; Setup lsp-mode with terraform-mode using terraform-ls

;;; Code:

(with-eval-after-load 'lsp-mode
  (defun my/lsp-terraform-setup ()
    "Setup LSP for terraform-mode."
    ;; garante que terraform-ls está no PATH
    (setq lsp-terraform-server-command '("terraform-ls" "serve"))
    ;; inicia lsp de forma adiada
    (lsp-deferred))

  (add-hook 'terraform-mode-hook #'my/lsp-terraform-setup))

(provide 'hooks-lsp-terraform)
;;; hooks-lsp-terraform.el ends here
