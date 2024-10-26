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
(global-hl-line-mode 1)
(delete-selection-mode t)
(electric-pair-mode 1)
(savehist-mode 1)
(save-place-mode 1)

;;; Keybindings
(global-set-key "\M-h" 'help-for-help)
(global-set-key [C-backspace] 'kill-whole-line)
(global-set-key (kbd "C-q") 'other-window)
(global-set-key (kbd "M-i") 'enlarge-window)
(global-set-key (kbd "M-k") 'shrink-window)
(global-set-key (kbd "M-j") 'enlarge-window-horizontally)
(global-set-key (kbd "M-l") 'shrink-window-horizontally)
(global-set-key (kbd "M-<f12>") 'nshell)

;; Disable ctrl+Z
(global-unset-key (kbd "C-z"))

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
(use-package which-key :ensure t :init (which-key-mode 1) :config (setq which-key-separator " → "))
(use-package nerd-icons :ensure t :config (setq nerd-icons-font-family "Symbols Nerd Font Mono"))
(use-package neotree :ensure t :bind (("M-\\" . neotree-toggle)) :config (setq neo-theme 'icons))
(use-package ace-window :ensure t :bind (("M-o" . ace-window)))
(use-package catppuccin-theme :ensure t)
(use-package autothemer :ensure t)

;;; Install theme from GitHub
(straight-use-package
 '(rose-pine-emacs :host github :repo "thongpv87/rose-pine-emacs" :branch "master"))
(load-theme 'rose-pine-moon t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  :config
  ;; Define o ruff como o verificador padrão para arquivos Python
  (flycheck-define-checker python-ruff
    "Um verificador para Python usando ruff."
    :command ("poetry" "run" "task" "ruff-lint"  source-inplace)
    :error-patterns
    ((error line-start (file-name) ":" line ":" (message) line-end))
    :modes python-mode)

  ;; Define o mypy como um verificador para arquivos Python
  (flycheck-define-checker python-mypy
    "Um verificador para Python usando mypy."
    :command ("poetry" "run" "task" "mypy-lint" source-original)
    :error-patterns
    ((warning line-start (file-name) ":" line ": " (message) line-end)
     (error line-start (file-name) ":" line ": error: " (message) line-end))
    :modes python-mode)

  ;; Habilita o checker ruff para Python e configura mypy como próximo verificador
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
(setq dashboard-banner-logo-title "I use emacs btw 🤓☝️")
  
;; Set the banner
(setq dashboard-startup-banner 'logo)

;; Config icons
(setq dashboard-set-heading-icons t
    dashboard-set-file-icons t)

;; Dashboard widgets
(setq dashboard-items '((recents  . 5)    ;; Arquivos recentes
                        (projects . 5)  ;; Bookmarks
                        (agenda . 5)     ;; Agenda do org-mode
                        (bookmarks . 5))) ;; Projetos recentes

;; Center layout
(setq dashboard-center-content t)

(setq dashboard-startupify-list '(dashboard-insert-banner
                                  dashboard-insert-newline
                                  dashboard-insert-banner-title
                                  dashboard-insert-newline
  				  dashboard-insert-navigator
                                  dashboard-insert-items
                                  dashboard-insert-newline
                                  dashboard-insert-footer
				  dashboard-insert-newline
				  dashboard-insert-init-info))

(setq dashboard-navigator-buttons
      `(
        (
          (,(nerd-icons-mdicon "nf-md-github" :height 1.1 :v-adjust 0.0)
           "Github"
           "Browse GitHub profile"
           (lambda (&rest _) (browse-url "https://github.com/Robert-Nogueira")))

          (,(nerd-icons-mdicon "nf-md-cog" :height 1.1 :v-adjust 0.0)
	   "Settings" "Open init.el"
           (lambda (&rest _) (find-file user-init-file)))
        )))

  (setq dashboard-footer-messages '("Strong coffee, strong code."))
  (setq dashboard-set-footer t)
  (setq dashboard-footer-icon
        (nerd-icons-mdicon "nf-md-coffee" :height 1.0 :v-adjust -0.05))

(set-face-attribute 'dashboard-banner-logo-title nil :height 180 :weight 'bold)
(set-face-foreground 'dashboard-banner-logo-title "#cba6f7")


;; Vertico
(use-package vertico
   :bind (:map vertico-map
               ("C-l" . vertico-next)
               ("C-j" . vertico-previous)
               ("C-f" . vertico-exit)
               :map minibuffer-local-map
               ("M-h" . backward-kill-word))
   :custom
   (vertico-cycle t)
   :init
   (vertico-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-lv
  :init
  (marginalia-mode))))
;; Ordeless
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

;;; Auto-complete faces customization
(set-face-background 'ac-candidate-face "#11111b")
(set-face-foreground 'ac-candidate-face "#89b4fa")
(set-face-background 'ac-selection-face "#cba6f7")
(set-face-foreground 'ac-selection-face "#11111b")
(set-face-foreground 'ac-completion-face "#cba6f7")

;;; auto-update-package
(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-interval 7)
  (autovupdate-prompt-before-update t
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "21:00")))

(setq backup-directory-alist '((".*" . "~/.saves")))

(setq-default electric-indent-inhibit t)

(defun my-python-format-on-save ()
  "Run 'poetry run task format' on save for Python files, skipping temporary files."
  (when (and (eq major-mode 'python-mode)
             (buffer-file-name)
             (not (string-match-p "\\.#.*" (buffer-file-name)))) ;; Ignora arquivos temporários
    (shell-command "echo antes && poetry run task format && echo teste")
    (revert-buffer :ignore-auto :noconfirm)))

(add-hook 'after-save-hook 'my-python-format-on-save)

;; (concat "cd " (projectile-project-root) " && poetry run task format")

(setq-default display-fill-column-indicator-column 79)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
