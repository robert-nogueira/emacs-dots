;;; misc.el --- Miscellaneous Configurations
;;; Commentary:
;; This file contains various package configurations that enhance
;; the Emacs experience, including utilities for navigation and
;; clipboard management.

;;; Code:

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :config
  (setq which-key-separator " → ")) ;; Customize the separator

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))) ;; Bind M-o to ace-window for quick navigation

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

(use-package counsel-spotify
  :ensure t
  :config
  (setq counsel-spotify-client-id "3660c85228374f47a0df42a32b8d730b")
    (setq counsel-spotify-client-secret "f3e5b2d0cef341d9b36819d996060653"))

(use-package yasnippet
  :ensure t
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

(provide 'misc)
;;; misc.el ends here
