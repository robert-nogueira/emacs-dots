;;; flycheck.el --- Flycheck Configuration for Python
;;; Commentary:

;; This file configures Flycheck to use Ruff and Mypy as checkers for Python files.
;; It also sets up a cache for Flycheck.

;;; Code:

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  :config
  (flycheck-define-checker python-ruff
    "A Python syntax checker using Ruff."
    :command ("poetry" "run" "task" "ruff-lint" source-inplace)
    :error-patterns
    ((error line-start (file-name) ":" line ":" (message) line-end))
    :modes python-mode)

  (flycheck-define-checker python-mypy
    "A Python type checker using MyPy."
    :command ("poetry" "run" "task" "mypy-lint" source-original)
    :error-patterns
    ((warning line-start (file-name) ":" line ": " (message) line-end)
     (error line-start (file-name) ":" line ": error: " (message) line-end))
    :modes python-mode)

  (add-hook 'python-mode-hook
            (lambda ()
              (flycheck-select-checker 'python-ruff)
              (flycheck-add-next-checker 'python-ruff 'python-mypy))))

(setq flycheck-checker-cache "~/.flycheck-cache")

(use-package flycheck-inline
  :ensure t)

(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))


(provide 'flycheck)
;;; flycheck.el ends here
