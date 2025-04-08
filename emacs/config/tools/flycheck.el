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
)

  ;; Define Ruff checker
  ;; (flycheck-define-checker python-ruff
  ;;   "A Python syntax checker using Ruff."
  ;;   :command ("ruff" "--quiet" "check" "--stdin-filename" source-inplace)
  ;;   :standard-input t
  ;;   :error-patterns
  ;;   ((error line-start (file-name) ":" line ":" (message) line-end))
  ;;   :modes python-mode)

  ;; ;; Define MyPy checker
  ;; (flycheck-define-checker python-mypy
  ;;   "A Python type checker using MyPy."
  ;;   :command ("mypy" "--show-error-codes" "--cache-dir"
  ;;             (eval (expand-file-name ".mypy_cache" (projectile-project-root)))
  ;;             "--config-file"
  ;;             (eval (expand-file-name "pyproject.toml" (projectile-project-root)))
  ;;             source-original)
  ;;   :standard-input t
  ;;   :error-patterns
  ;;   ((warning line-start (file-name) ":" line ": " (message) line-end)
  ;;    (error line-start (file-name) ":" line ": error: " (message) line-end))
  ;;   :modes python-mode)

  ;; ;; Hook para selecionar os checkers
  ;; (add-hook 'python-mode-hook
  ;;           (lambda ()
  ;;             (flycheck-select-checker 'python-ruff)
  ;;             (flycheck-add-next-checker 'python-ruff 'python-mypy))))


(setq flycheck-checker-cache "~/.flycheck-cache")

(setq flycheck-indication-mode nil)

(add-hook 'python-mode-hook #'flycheck-mode)
;; (add-hook 'poetry-after-pipenv-activate-hook
;;           (lambda ()
;;             (when (derived-mode-p 'python-mode)
;;               (flycheck-buffer))))  ;; Força o Flycheck a verificar após o Poetry concluir

(custom-set-faces
 '(flycheck-error ((t (:underline (:color "#cba6f7" :style line) :weight normal))))
 '(flycheck-warning ((t (:underline (:color "#b4befe" :style line) :weight normal))))
 '(flycheck-info ((t (:underline (:color "#b4befe" :style line) :weight normal))))
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(bm-face ((t (:background "#cba6f7" :foreground "#11111b"))))
 '(bm-persistent-face ((t (:background "#cba6f7" :foreground "#11111b"))))
 ;; '(flycheck-error ((t (:underline (:color "#cba6f7" :style line)))))
 ;; '(flycheck-fringe-error ((t (:background nil :foreground nil))))
 ;; '(flycheck-fringe-info ((t (:background nil :foreground nil))))
 ;; '(flycheck-info ((t (:underline (:color "#00ff00" :style wave)))))
 ;; '(flycheck-warning ((t (:underline (:color "#89b4fa" :style line)))))
 '(treemacs-git-added-face ((t (:foreground "#94e2d5" :weight bold))))
 '(treemacs-git-modified-face ((t (:foreground "#cba6f7" :weight bold))))
 '(treemacs-git-untracked-face ((t (:foreground "#f38ba8" :weight bold)))))
(custom-set-faces
  '(font-lock-unused-variable-face ((t (:foreground "#ff0000" :weight bold :background "#f0f0f0"))))
)

(provide 'flycheck)
;;; flycheck.el ends here
