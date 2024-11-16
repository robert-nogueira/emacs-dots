;;; flycheck.el --- Flycheck Configuration for Python
;;; Commentary:

;; This file configures Flycheck to use Ruff and Mypy as checkers for Python files.
;; It also sets up a cache for Flycheck.

;;; Code:

(use-package flycheck
  :ensure t
  :after poetry
  :demand t
  :init
  (global-flycheck-mode t)
  :config
  (flycheck-define-checker python-ruff
    "A Python syntax checker using Ruff."
    :command ("ruff" "check" "--stdin-filename" source-original)
  :standard-input t
  :error-patterns
    ((error line-start (file-name) ":" line ":" (message) line-end))
    :modes python-mode)

  (flycheck-define-checker python-mypy
    "A Python type checker using MyPy."
    :command ("mypy" "--strict" "--show-error-codes" "--cache-dir"
              (eval (expand-file-name ".mypy_cache" (projectile-project-root)))
              source-original)
    :standard-input t
    :error-patterns
    ((warning line-start (file-name) ":" line ": " (message) line-end)
     (error line-start (file-name) ":" line ": error: " (message) line-end))
    :modes python-mode)

  (add-hook 'python-mode-hook
            (lambda ()
              (flycheck-select-checker 'python-ruff)
              (flycheck-add-next-checker 'python-ruff 'python-mypy))))

(flycheck-def-config-file-var flycheck-python-ruff-config python-ruff
                              '("pyproject.toml" "ruff.toml" ".ruff.toml"))

(setq flycheck-checker-cache "~/.flycheck-cache")

(use-package flycheck-inline
  :ensure t)

(setq flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook 'flycheck-inline-mode))

(custom-set-faces
 '(flycheck-fringe-error ((t (:background nil :foreground nil))))
 '(flycheck-fringe-error ((t (:background nil :foreground nil))))
 '(treemacs-git-added-face ((t (:foreground "#94e2d5" :weight bold))))
 '(treemacs-git-modified-face ((t (:foreground "#cba6f7" :weight bold))))
 '(treemacs-git-untracked-face ((t (:foreground "#f38ba8" :weight bold)))))

(provide 'flycheck)
;;; flycheck.el ends here
