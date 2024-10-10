;;; Commentary:
;; Configuration file for Emacs setup and packages.

;; Disable welcome message
(setq inhibit-startup-message t)

;;; Package management setup
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         (or (bound-and-true-p straight-base-dir)
                             user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq straight-use-package-by-default t)

;;; UI Adjustments
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq-default cursor-type 'bar)
(setq scroll-step 1
      scroll-conservatively 10000)
(global-display-line-numbers-mode 1)
(setq ring-bell-function 'ignore)
(setq select-enable-clipboard t)

;;; Keybindings
(global-set-key "\M-h" 'help-for-help)
(global-set-key [C-backspace] 'kill-whole-line)
(global-set-key (kbd "C-q") 'other-window)
(global-set-key (kbd "M-i") 'enlarge-window)
(global-set-key (kbd "M-k") 'shrink-window)
(global-set-key (kbd "M-j") 'enlarge-window-horizontally)
(global-set-key (kbd "M-l") 'shrink-window-horizontally)
(global-set-key (kbd "M-<f12>") 'nshell)

;;; Functions
(defun nshell()
  "Create a shell in another window vertically."
  (interactive)
  (let ((buf (shell)))
    (switch-to-buffer (other-buffer buf))
    (let ((win (split-window-below)))
      (set-window-buffer win buf)
      (select-window win))))

;;; Fix backspace issue
(setq emacs-startup-hook
      (lambda ()
        (setq keyboard-translate-table "\C-@\C-a\C-b\C-c\C-d\C-e\C-f\C-g\C-?")))

;;; Packages
(use-package try :ensure t)
(use-package which-key :ensure t :config (which-key-mode))
(use-package nerd-icons :ensure t :config (setq nerd-icons-font-family "Symbols Nerd Font Mono"))
(use-package neotree :ensure t :bind (("M-q" . neotree-toggle)) :config (setq neo-theme 'icons))
(use-package ace-window :ensure t :bind (("M-o" . ace-window)))
(use-package catppuccin-theme :ensure t)
(use-package autothemer :ensure t)

;;; Install theme from GitHub
(straight-use-package
 '(rose-pine-emacs :host github :repo "thongpv87/rose-pine-emacs" :branch "master"))
(load-theme 'rose-pine-moon t)

;;; Flycheck configuration
(use-package flycheck :ensure t :init (global-flycheck-mode t))
(use-package flycheck-inline :ensure t)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

;;; Python environment
(use-package jedi :ensure t :init (add-hook 'python-mode-hook 'jedi:setup) 
  (add-hook 'python-mode-hook 'jedi:ac-setup)
  (setq jedi:use-shortcuts t jedi:complete-on-dot t))
(use-package poetry :ensure t :hook (python-mode . poetry-tracking-mode))
(use-package toml-mode :ensure t)

;;; Discord Rich Presence
(use-package elcord :ensure t)
(elcord-mode)
(setq elcord-idle-timer 5)
(setq elcord-idle-message "Tomando uma brejinha...")

;;; Web development
(use-package web-mode :ensure t)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

;;; Enable clipboard support
(use-package xclip :ensure t)
(xclip-mode 1)

;;; Auto-complete configuration
(use-package auto-complete :ensure t :init (ac-config-default) (global-auto-complete-mode t))

;;; Version control with Magit
(use-package magit :ensure t)

;;; Highlight changes
(use-package diff-hl :ensure t)
(global-diff-hl-mode)

;;; Dashboard setup
(use-package dashboard :ensure t :config (dashboard-setup-startup-hook))
(setq dashboard-banner-logo-title "Pai coda dms não tem como")
(setq dashboard-items '((recents   . 5) (projects  . 5)))
(setq dashboard-display-icons-p t)
(setq dashboard-icon-type 'nerd-icons)

;;; Auto-complete faces customization
(set-face-background 'ac-candidate-face "#11111b")
(set-face-foreground 'ac-candidate-face "#89b4fa")
(set-face-background 'ac-selection-face "#cba6f7")
(set-face-foreground 'ac-selection-face "#11111b")
(set-face-foreground 'ac-completion-face "#cba6f7")

;;; Custom variables and faces
(custom-set-variables
 '(package-selected-packages
   '(flycheck-inline xclip which-key web-mode try toml-mode poetry nerd-icons neotree magit lsp-ui lsp-pyright lsp-jedi jedi flycheck elcord diff-hl company-jedi catppuccin-theme all-the-icons ace-window)))
(custom-set-faces)
