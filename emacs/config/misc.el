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

(message "Spotify Client ID: %s" (getenv "SPOTIFY_CLIENT_ID"))
(message "Spotify Client Secret: %s" (getenv "SPOTIFY_CLIENT_SECRET"))


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
  (setq counsel-spotify-client-id (getenv "SPOTIFY_CLIENT_ID"))
  (setq counsel-spotify-client-secret (getenv "SPOTIFY_CLIENT_SECRET")))

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
