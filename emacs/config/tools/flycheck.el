;;; flycheck.el --- Flycheck Configuration for Python
;;; Commentary:

;; Configure Flycheck with Ruff and Mypy, ensuring error display works immediately.

;;; Code:

(use-package flycheck
  :ensure t
  ;; :after poetry
  ;; :demand t
  :init
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))
  (setq flycheck-idle-change-delay 0)
  :config
  (global-flycheck-mode t)

  ;; Define Ruff checker
  (flycheck-define-checker python-ruff
    "A Python syntax checker using Ruff."
    :command ("ruff" "--quiet" "check" "--stdin-filename" source-inplace)
    :standard-input t
    :error-patterns
    ((error line-start (file-name) ":" line ":" (message) line-end))
    :modes python-mode)

  ;; Define MyPy checker
  (flycheck-define-checker python-mypy
    "A Python type checker using MyPy."
    :command ("mypy" "--strict" "--show-error-codes" "--cache-dir"
              (eval (expand-file-name ".mypy_cache" (projectile-project-root)))
              "--config-file"
              (eval (expand-file-name "pyproject.toml" (projectile-project-root)))
              source-original)
    :standard-input t
    :error-patterns
    ((warning line-start (file-name) ":" line ": " (message) line-end)
     (error line-start (file-name) ":" line ": error: " (message) line-end))
    :modes python-mode)

  ;; Hook para selecionar os checkers
  (add-hook 'python-mode-hook
            (lambda ()
              (flycheck-select-checker 'python-ruff)
              (flycheck-add-next-checker 'python-ruff 'python-mypy))))


(use-package flycheck-inline
  :ensure t
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-inline-mode))

(setq flycheck-checker-cache "~/.flycheck-cache")

(setq flycheck-indication-mode nil)

(add-hook 'python-mode-hook #'flycheck-mode)
;; (add-hook 'poetry-after-pipenv-activate-hook
;;           (lambda ()
;;             (when (derived-mode-p 'python-mode)
;;               (flycheck-buffer))))  ;; Força o Flycheck a verificar após o Poetry concluir

(custom-set-faces
 '(flycheck-error ((t (:underline (:color "#cba6f7" :style line)))))
 '(flycheck-warning ((t (:underline (:color "#89b4fa" :style line)))))
 '(flycheck-info ((t (:underline (:color "#00ff00" :style wave))))))

(provide 'flycheck)
;;; flycheck.el ends here
