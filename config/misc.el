;;; misc.el --- Miscellaneous Configurations
;;; Commentary:
;; This file contains various package configurations that enhance
;; the Emacs experience, including utilities for navigation and
;; clipboard management.

;;; Code:

(defun load-env-file (filepath)
  "Load .env and define in Emacs environment from FILEPATH."
  (when (file-exists-p filepath)
    (with-temp-buffer
      (insert-file-contents filepath)
      (dolist (line (split-string (buffer-string) "\n" t))
        (let ((parts (split-string line "=")))
          (when (= (length parts) 2)
            (setenv (car parts) (cadr parts))))))))

(load-env-file "~/.emacs.d/.env")



(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :config
  (setq which-key-separator " → "))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

(use-package counsel-spotify
  :ensure t
  :config
  (setq counsel-spotify-client-id (getenv "SPOTIFY_CLIENT_ID"))
  (setq counsel-spotify-client-secret (getenv "SPOTIFY_CLIENT_SECRET")))

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode);
  :config
  (yas-global-mode 1))

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(setq keyfreq-excluded-commands
      '(self-insert-command
	mouse-wheel-text-scale
	mwheel-scroll
	vertico-exit
	delete-backward-char
        forward-char
        backward-char
        previous-line
        next-line
	vertico-next
	vertico-insert))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "pandoc")
  (setq markdown-preview-stylesheets (list "~/.emacs.d/github-markdown-dark.css"))
  )

(use-package markdown-preview-eww
  :ensure t
  :after markdown-mode)

(use-package markdown-preview-mode
  :ensure t)

(use-package centered-cursor-mode
  :ensure t
  :init
  (global-centered-cursor-mode 1)
  )

(use-package expand-region
  :ensure t
  :bind ("C-9" . er/expand-region))

(use-package emojify
  :hook (prog-mode . emojify-mode))

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
	    '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  )

(use-package aggressive-indent
  :ensure t)
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-auto-enabled nil)
  (highlight-indent-guides-method 'character)
  ;; (highlight-indent-guides-character ?ǀ) ;; Credit: https://github.com/SwiftyChicken/dotfiles/blob/7cb2f117d722300d36ce4d0e4ad205f7cb758275/.config/emacs/local/etc/code/prog.el#L21
  (highlight-indent-guides-character nil)
  (highlight-indent-guides-responsive 'stack)
  :hook

  (prog-mode . highlight-indent-guides-mode)
  :config
  (set-face-attribute 'highlight-indent-guides-odd-face nil :background "#7c6fbe")
  (set-face-attribute 'highlight-indent-guides-even-face nil :background "#7c6fbe")
  (set-face-attribute 'highlight-indent-guides-character-face nil :foreground "#7c6fbe")
  (set-face-attribute 'highlight-indent-guides-stack-odd-face nil :background "#7c6fbe")
  (set-face-attribute 'highlight-indent-guides-stack-even-face nil :background "#7c6fbe")
  (set-face-attribute 'highlight-indent-guides-stack-character-face nil :foreground "#7c6fbe")
  (set-face-attribute 'highlight-indent-guides-top-odd-face nil :background "#9ab4ff")
  (set-face-attribute 'highlight-indent-guides-top-even-face nil :background "#9ab4ff")
  (set-face-attribute 'highlight-indent-guides-top-character-face nil :foreground "#9ab4ff")
  )

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#f38ba8")
          ("FIXME"  . "#f38ba8")
          ("DEBUG"  . "#cba6f7")
          ("GOTCHA" . "#fab387")
          ("STUB"   . "#89b4fa"))))

(provide 'misc)
;;; misc.el ends here
