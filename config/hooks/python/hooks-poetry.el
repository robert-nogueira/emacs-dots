;;; hooks-poetry.el --- Poetry integration for python-mode

;;; Commentary:
;; Setup poetry venv detection and LSP deferred in python-mode.

;;; Code:

(with-eval-after-load 'poetry
  (defun my/python-poetry-setup ()
    (when (poetry-venv-exist-p)
      (message "poetry-venv-exist-p: %s" (symbol-value 'poetry-venv-exist-p))
      (lsp-deferred)))

  (add-hook 'python-mode-hook #'my/python-poetry-setup))

(provide 'hooks-poetry)
;;; hooks-poetry.el ends here
