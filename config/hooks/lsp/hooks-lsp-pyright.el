;;; hooks-lsp-pyright.el --- lsp-pyright hook configuration

;;; Commentary:
;; Setup lsp-pyright for python-mode

;;; Code:

(with-eval-after-load 'lsp-pyright
  (defun my/lsp-pyright-python-setup ()
    (setq lsp-pyright-venv-path
          (string-trim (shell-command-to-string "poetry env info -p")))
    (lsp-deferred))

  (add-hook 'python-mode-hook #'my/lsp-pyright-python-setup))

(provide 'hooks-lsp-pyright)
;;; hooks-lsp-pyright.el ends here
