;;; hooks.el --- Configuration for various hooks in Emacs

;;; Commentary:
;; This file configures various hooks in Emacs for different packages.

;;; Code:

;; Treemacs
(with-eval-after-load 'treemacs
  (add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'treemacs-mode-hook #'treemacs-project-follow-mode))

;; Centaur Tabs
(with-eval-after-load 'centaur-tabs
  (add-hook 'dired-mode-hook 'centaur-tabs-local-mode))

;; Python (Ligature)
(with-eval-after-load 'python
  (with-eval-after-load 'ligature
    (add-hook 'python-mode-hook
              (lambda ()
                (ligature-set-ligatures 'python-mode '("->" "=>" "==" "!=" ">=" "<="))
                (ligature-mode 1)))))

;; Diff-hl
(with-eval-after-load 'diff-hl
  (add-hook 'after-save-hook #'diff-hl-update)
  
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; VTerm
(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1))))

;; Python (Company, Line Numbers, Font-lock, Poetry)
(with-eval-after-load 'python
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local company-backends
                          '(company-capf company-dabbrev))))
  (add-hook 'python-mode-hook 'display-line-numbers-mode)
  (add-hook 'python-mode-hook #'font-lock-mode)

  (with-eval-after-load 'poetry
    (add-hook 'python-mode-hook
              (lambda ()
                (when (poetry-venv-exist-p)
                  (message "poetry-venv-exist-p: %s"
                           (symbol-value 'poetry-venv-exist-p))
                  (lsp-deferred))))))

;; LSP Mode
(with-eval-after-load 'lsp-mode
  (add-hook 'python-mode-hook 'lsp)
  (define-key lsp-mode-map (kbd "C-c r") #'lsp-rename))

;; LSP Pyright
(with-eval-after-load 'lsp-pyright
  (add-hook 'python-mode-hook
            (lambda ()
              (require 'lsp-pyright)
              (setq lsp-pyright-venv-path
                    (string-trim
                     (shell-command-to-string
                      "poetry env info -p")))
              (lsp-deferred))))

;; LSP UI
(with-eval-after-load 'lsp-ui
  (define-key lsp-ui-mode-map
    [remap xref-find-definitions]
    #'lsp-ui-peek-find-definitions)

  (define-key lsp-ui-mode-map
    [remap xref-find-references]
    #'lsp-ui-peek-find-references)

  (global-set-key (kbd "C-c C-d") 'lsp-ui-doc-show)

(provide 'hooks)
;;; hooks.el ends here
