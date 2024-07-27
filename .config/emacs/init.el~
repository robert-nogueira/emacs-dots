;; Commentary:
;; Your comments here...

;; Remove welcome message
(setq inhibit-startup-message t)

;; Create a shell in another window vertically
(defun nshell()
  "Create a shell in another window vertically."
  (interactive)
  (let ((buf (shell)))
    (switch-to-buffer (other-buffer buf))
    (let ((win (split-window-below)))
      (set-window-buffer win buf)
      (select-window win))))

;; Remove menus
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Enable line numbers
(global-display-line-numbers-mode 1)

;; Fix backspace
(setq term-setup-hook
      (lambda ()
        (setq keyboard-translate-table "\C-@\C-a\C-b\C-c\C-d\C-e\C-f\C-g\C-?")))

;; Set keybindings
(global-set-key "\M-h" 'help-for-help)
(global-set-key [C-backspace] 'kill-whole-line)
(global-set-key (kbd "C-q") 'other-window)
(global-set-key (kbd "M-i") 'enlarge-window)
(global-set-key (kbd "M-k") 'shrink-window)
(global-set-key (kbd "M-j") 'enlarge-window-horizontally)
(global-set-key (kbd "M-l") 'shrink-window-horizontally)
(global-set-key (kbd "M-<f12>") 'nshell)

;; Disable ring bell sound
(setq ring-bell-function 'ignore)

;; Enable clipboard support
(setq x-select-enable-clipboard t)

;; Package management
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Packages configuration
(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package auto-complete
  :ensure t
  :init
  (ac-config-default)
  (global-auto-complete-mode t))

(use-package nerd-icons
  :ensure t
  :config
  (setq nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'nerd)
  :bind (("M-q" . neotree-toggle)))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

(use-package catppuccin-theme
  :ensure t)
(load-theme 'catppuccin :no-confirm)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))

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
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

(use-package xclip
  :ensure t)
(xclip-mode 1)

;; Custom variables and faces
(custom-set-variables
 '(elcord-idle-message "Tomando uma brejinha...")
 '(elcord-idle-timer 20)
 '(package-selected-packages '(neotree auto-complete try)))

(custom-set-faces
 ;; Custom faces here...
 )
;;; Comentaryc: ...

;; Remove welcome message
(setq inhibit-startup-message t)


(defun nshell()
  "Create a shell in another window vertically"
  (interactive)
  (let ((buf (shell)))
    (switch-to-buffer (other-buffer buf))
    (let ((win (split-window-below)))
      (set-window-buffer win buf)
      (select-window win))))

;; Remove menus
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Line numbers
(global-display-line-numbers-mode 1)

;; Fix backspace
(setq term-setup-hook
      (lambda ()
        (setq keyboard-translate-table "\C-@\C-a\C-b\C-c\C-d\C-e\C-f\C-g\C-?")))

(global-set-key "\M-h" 'help-for-help)

;; Configure backspace
(global-set-key [C-backspace] 'kill-whole-line)

;; Packages
(require 'package)
(setq package-enable-at-startup nil)

;; disable ring bell sound
(setq ring-bell-function 'ignore)

(setq x-select-enable-clipboard t)

;; MELPA repo
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package auto-complete
  :ensure t
  :init
  (ac-config-default)
  (global-auto-complete-mode t))

(use-package nerd-icons
  :ensure t
  :config
  (setq nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'nerd)
  :bind (("M-q" . neotree-toggle)))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

;; theme
(use-package catppuccin-theme
  :ensure t)
(load-theme 'catppuccin :no-confirm)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode) t)


(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup)
  (setq jedi:use-shortcuts t)
 )

(setq jedi:complete-on-dot t)

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
                   ("beg" "end")))
	))

(use-package xclip
 :ensure t)
(xclip-mode 1)

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-ac-sources-alist
  '(("css" . (ac-source-css-property))
    ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

;; setting keybinds
(global-set-key (kbd "C-q") 'other-window)
(global-set-key (kbd "M-i") 'enlarge-window)
(global-set-key (kbd "M-k") 'shrink-window)
(global-set-key (kbd "M-j") 'enlarge-window-horizontally)
(global-set-key (kbd "M-l") 'shrink-window-horizontally)

;; MELPA stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elcord-idle-message "Tomando uma brejinha...")
 '(elcord-idle-timer 20)
 '(package-selected-packages '(neotree auto-complete try)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

