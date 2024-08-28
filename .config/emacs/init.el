;;; Commentary:
;; Configuration file for Emacs setup and packages.

;; Disable welcome message
(setq inhibit-startup-message t)

;; smoth scroll
(setq scroll-step            1
      scroll-conservatively  10000)

(setq-default cursor-type 'bar)

;; Function to create a shell in another window vertically
(defun nshell()
  "Create a shell in another window vertically."
  (interactive)
  (let ((buf (shell)))
    (switch-to-buffer (other-buffer buf))
    (let ((win (split-window-below)))
      (set-window-buffer win buf)
      (select-window win))))

;; UI adjustments
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Line numbers
(global-display-line-numbers-mode 1)

;; Fix backspace issue
(setq emacs-startup-hook
      (lambda ()
        (setq keyboard-translate-table "\C-@\C-a\C-b\C-c\C-d\C-e\C-f\C-g\C-?")))

;; Keybindings
(global-set-key "\M-h" 'help-for-help)
(global-set-key [C-backspace] 'kill-whole-line)
(global-set-key (kbd "C-q") 'other-window)
(global-set-key (kbd "M-i") 'enlarge-window)
(global-set-key (kbd "M-k") 'shrink-window)
(global-set-key (kbd "M-j") 'enlarge-window-horizontally)
(global-set-key (kbd "M-l") 'shrink-window-horizontally)
(global-set-key (kbd "M-<f12>") 'nshell)

;; Package management
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Disable bell sound
(setq ring-bell-function 'ignore)

;; Enable clipboard
(setq select-enable-clipboard t)

;; Packages
(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package nerd-icons
  :ensure t
  :config
  (setq nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'icons)
  :bind (("M-q" . neotree-toggle)))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

;; Theme
(use-package catppuccin-theme
  :ensure t)
(load-theme 'catppuccin :no-confirm)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))

(use-package flycheck-inline
  :ensure t)

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup)
  (setq jedi:use-shortcuts t)
  (setq jedi:complete-on-dot t))

(use-package poetry
  :ensure t)

(use-package toml-mode
  :ensure t)

(use-package elcord
  :ensure t)
(elcord-mode)
(setq elcord-idle-timer 1)
(setq elcord-idle-message "Tomando uma brejinha...")

(use-package web-mode
  :ensure t)
(setq web-mode-extra-auto-pairs
      '(("erb"  . (("beg" "end")))
        ("php"  . (("beg" "end")
                   ("beg" "end")))))

(use-package xclip
 :ensure t)
(xclip-mode 1)

(use-package auto-complete
  :ensure t
  :init
  (ac-config-default)
  (global-auto-complete-mode t))
(use-package magit
  :ensure t)

(use-package diff-hl
  :ensure t)
(global-diff-hl-mode)

(use-package poetry
  :ensure t
  :hook
  ;; activate poetry-tracking-mode when python-mode is active
  (python-mode . poetry-tracking-mode)
)

;; use-package with package.el:
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq dashboard-banner-logo-title "Pai coda dms não tem como")
(setq dashboard-items '((recents   . 5)
                        (projects  . 5)))

(setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
(setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

;; Auto-complete faces customization
(set-face-background 'ac-candidate-face "#11111b")
(set-face-foreground 'ac-candidate-face "#89b4fa")
(set-face-background 'ac-selection-face "#cba6f7")
(set-face-foreground 'ac-selection-face "#11111b")
(set-face-foreground 'ac-completion-face "#cba6f7")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(flycheck-inline xclip which-key web-mode try toml-mode poetry nerd-icons neotree magit lsp-ui lsp-pyright lsp-jedi jedi flycheck elcord diff-hl company-jedi catppuccin-theme all-the-icons ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
