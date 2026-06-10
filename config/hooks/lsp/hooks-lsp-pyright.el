;;; hooks-lsp-pyright.el --- lsp-pyright hook configuration

;;; Commentary:
;; Setup lsp-pyright for python-mode

;;; Code:

(with-eval-after-load 'lsp-pyright
  (defun my/lsp-pyright-python-setup ()
    (let ((project-root (locate-dominating-file default-directory "pyproject.toml")))
      (if (not project-root)
          (setq-local lsp-pyright-venv-path nil)
        (let* ((default-directory project-root)
               (output (string-trim
                        (shell-command-to-string "poetry env info -p 2>&1"))))
          (if (file-directory-p output)
              (setq-local lsp-pyright-venv-path output)
            (progn
              (setq-local lsp-pyright-venv-path nil)
              (lsp--warn
               "lsp-pyright: `poetry env info -p` não retornou um venv válido em %s (saída: %S). Pyright vai usar o Python padrão."
               project-root output))))))
    (lsp-deferred))

  (add-hook 'python-mode-hook #'my/lsp-pyright-python-setup))

(defun my/lsp-pyright-debug ()
  "Show diagnostic information about the venv used by lsp-pyright."
  (interactive)
  (let* ((orig-buffer (current-buffer))
         (venv-path (buffer-local-value 'lsp-pyright-venv-path orig-buffer))
         (project-root (or (locate-dominating-file default-directory "pyproject.toml")
                            default-directory))
         (default-directory project-root)
         (env-info (shell-command-to-string "poetry env info 2>&1"))
         (python-version (shell-command-to-string "pyenv version 2>&1")))
    (with-current-buffer (get-buffer-create "*lsp-pyright-debug*")
      (erase-buffer)
      (insert (format "project-root: %s\n\n" project-root))
      (insert (format "lsp-pyright-venv-path (buffer-local): %S\n\n" venv-path))
      (insert "pyenv version:\n") (insert python-version) (insert "\n")
      (insert "poetry env info:\n") (insert env-info)
      (display-buffer (current-buffer)))))

(provide 'hooks-lsp-pyright)
;;; hooks-lsp-pyright.el ends here
