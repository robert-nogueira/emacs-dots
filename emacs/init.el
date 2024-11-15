;; init.el --- Emacs Initialization File
;;; Commentary:
;; This file loads all necessary configurations for Emacs.

;;; Code:

(use-package async
  :ensure t
  :config (dired-async-mode 1))


(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 800000)))


(defvar config-dir (expand-file-name "config" user-emacs-directory))

(add-to-list 'load-path config-dir)

;; Load core modules
(load (expand-file-name "ui/interface" config-dir))
(load (expand-file-name "core/packages" config-dir))
(load (expand-file-name "core/keybindings" config-dir))
(load (expand-file-name "core/settings" config-dir))

;; Load UI modules
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))
(setq projectile-project-root-files '(".git" "pyproject.toml"))
(setq projectile-project-search-path '("~/Documents/freelas" "~/Documents/my-projects" "~/Documents/squarecloud")) ;; Diretórios com seus projetos

(load (expand-file-name "ui/dashboard" config-dir))
(load (expand-file-name "ui/treemacs" config-dir))
(load (expand-file-name "ui/theme" config-dir))

;; Load programming modules
(load (expand-file-name "tools/vc" config-dir))
(load (expand-file-name "programming/python" config-dir))
(load (expand-file-name "programming/web-dev" config-dir))

;; Load completion modules
(load (expand-file-name "completion/company" config-dir))
(load (expand-file-name "completion/vertico" config-dir))
(load (expand-file-name "completion/marginalia" config-dir))
(load (expand-file-name "completion/orderless" config-dir))

;; ;; Load tool modules
(load (expand-file-name "tools/centaur-tabs" config-dir))
(load (expand-file-name "tools/flycheck" config-dir))
(load (expand-file-name "tools/functions" config-dir))
(load (expand-file-name "tools/discord" config-dir))

;; Load miscellaneous configurations
(load (expand-file-name "misc" config-dir))
(load (expand-file-name "aliases" config-dir))

;; (use-package telephone-line
;;   :ensure t)

;; (telephone-line-mode t)

;; ;; Cores da modeline usando o tema Catppuccin Mocha com roxo
;; (set-face-background 'mode-line "#1E1E2E")         ; Cor de fundo ativa
;; (set-face-foreground 'mode-line "#F5E0DC")         ; Cor do texto ativo
;; (set-face-background 'mode-line-inactive "#26233A") ; Cor de fundo inativa
;; (set-face-foreground 'mode-line-inactive "#C6C6D5") ; Cor do texto inativo
;; (set-face-foreground 'mode-line '#DDB6F2)           ; Cor do texto ativo com tom de roxo

(display-time-mode 1)

;;; init.el ends here
;; (set-frame-parameter nil 'alpha-background 90)
;; (add-to-list 'default-frame-alist '(alpha-background . 90))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "7bcbee7c5eaef0eca0dad6c5ec0ee8f665efd857be421a67be1ce84003d71417" "11feb87b02688866cef2199e268cad5f6d473ebacaa5f06c35c3ac08894a2845" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(use-package vterm
  :config
  (setq vterm-kill-buffer-on-exit t)
  :ensure t)
(global-set-key (kbd "C-c C-t") 'vterm)
(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0)))

(use-package vterm-toggle
  :ensure t
  :bind (("M-<f12>" . vterm-toggle)
         ("C-c j" . vterm-toggle-forward)
         ("C-c l" . vterm-toggle-backward)))
