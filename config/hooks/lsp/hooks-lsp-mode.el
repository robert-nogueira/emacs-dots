;;; hooks-lsp.el --- LSP mode hook configuration

;;; Commentary:
;; Enable LSP in python and dockerfile modes and keybindings.

;;; Code:

(with-eval-after-load 'lsp-mode
  (add-hook 'python-mode-hook 'lsp)
  (add-hook 'dockerfile-mode-hook 'lsp)
  (define-key lsp-mode-map (kbd "C-c r") #'lsp-rename))

(provide 'hooks-lsp)
;;; hooks-lsp.el ends here
