;;; hooks.el --- Configuration for various hooks in Emacs

;;; Commentary:
;; This file configures various hooks in Emacs for different packages.

;;; Code:

;; treemacs
(with-eval-after-load 'treemacs
  (add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1))))

;; centaur tabs
(with-eval-after-load 'centaur-tabs
  (add-hook 'dired-mode-hook 'centaur-tabs-local-mode))

;; python (ligature, company, line numbers, font-lock, poetry)
(with-eval-after-load 'python
  ;; ligature
  (with-eval-after-load 'ligature
    (add-hook 'python-mode-hook
              (lambda ()
                (ligature-set-ligatures 'python-mode '("->" "=>" "==" "!=" ">=" "<="))
                (ligature-mode 1))))

  ;; company backend setup
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local company-backends
                          '(company-capf company-dabbrev))))

  ;; display line numbers and font-lock
  (add-hook 'python-mode-hook 'display-line-numbers-mode)
  (add-hook 'python-mode-hook #'font-lock-mode)

  ;; poetry (when poetry-venv exists, activate lsp)
  (with-eval-after-load 'poetry
    (add-hook 'python-mode-hook
              (lambda ()
                (when (poetry-venv-exist-p)
                  (message "poetry-venv-exist-p: %s"
                           (symbol-value 'poetry-venv-exist-p))
                  (lsp-deferred))))))

;; diff-hl
(with-eval-after-load 'diff-hl
  (add-hook 'after-save-hook #'diff-hl-update)

  ;; magit
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; vterm
(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1))))

;; lsp mode
(with-eval-after-load 'lsp-mode
  (add-hook 'python-mode-hook 'lsp)
  (add-hook 'dockerfile-mode-hook 'lsp)
  (define-key lsp-mode-map (kbd "C-c r") #'lsp-rename))

;; lsp pyright
(with-eval-after-load 'lsp-pyright
  (add-hook 'python-mode-hook
            (lambda ()
              (require 'lsp-pyright)
              (setq lsp-pyright-venv-path
                    (string-trim
                     (shell-command-to-string
                      "poetry env info -p")))
              (lsp-deferred))))

;; lsp ui
(with-eval-after-load 'lsp-ui
  (define-key lsp-ui-mode-map
	      [remap xref-find-definitions]
	      #'lsp-ui-peek-find-definitions)

  (define-key lsp-ui-mode-map
	      [remap xref-find-references]
	      #'lsp-ui-peek-find-references)

  (global-set-key (kbd "C-c C-d") 'lsp-ui-doc-show))

(with-eval-after-load 'dockerfile-mode
  (add-hook 'dockerfile-mode-hook
            (lambda ()
              (aggressive-indent-mode -1))))

(provide 'hooks)
;;; hooks.el ends here
