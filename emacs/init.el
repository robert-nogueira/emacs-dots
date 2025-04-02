;; init.el --- Emacs Initialization File
;;; Commentary:
;; This file loads all necessary configurations for Emacs.

;;; Code:

;; Set garbage collection threshold for faster startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 800000)))

;; Define config directory
(defvar config-dir (expand-file-name "config" user-emacs-directory))

;; Add config directory to load-path
(add-to-list 'load-path config-dir)

;; Load core modules first (basic setup)
(load (expand-file-name "core/packages" config-dir))
(load (expand-file-name "core/keybindings" config-dir))
(load (expand-file-name "core/settings" config-dir))

;; Load UI modules (interface, theme, etc.)
(load (expand-file-name "ui/interface" config-dir))
(load (expand-file-name "ui/theme" config-dir))
(load (expand-file-name "ui/dashboard" config-dir))

;; Load completion modules (company, vertico, etc.)
(load (expand-file-name "completion/company" config-dir))
(load (expand-file-name "completion/vertico" config-dir))
(load (expand-file-name "completion/marginalia" config-dir))
(load (expand-file-name "completion/orderless" config-dir))

;; Load programming-related modules
(load (expand-file-name "programming/python" config-dir))
(load (expand-file-name "programming/web-dev" config-dir))

;; Load tool modules (treemacs, centaur-tabs, etc.)
(load (expand-file-name "tools/treemacs" config-dir))
(load (expand-file-name "tools/centaur-tabs" config-dir))
(load (expand-file-name "tools/flycheck" config-dir))
(load (expand-file-name "tools/projectile" config-dir))
(load (expand-file-name "tools/functions" config-dir))
(load (expand-file-name "tools/vterm" config-dir))
(load (expand-file-name "tools/discord" config-dir))
(load (expand-file-name "tools/vc" config-dir))
(load (expand-file-name "tools/ligature" config-dir))

;; Load miscellaneous configurations
(load (expand-file-name "misc" config-dir))
(load (expand-file-name "aliases" config-dir))

;;; init.el ends here
;; (setq treemacs-directory-icon (nerd-icons-octicon "file-directory" :height 1.2 :face `(:foreground "#cba6f7")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-safe-themes
   '("8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a"
     "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "1ad12cda71588cc82e74f1cabeed99705c6a60d23ee1bb355c293ba9c000d4ac"
     "fae5872ff90462502b3bedfe689c02d2fa281bc63d33cb007b94a199af6ccf24"
     "c46651ab216eb31e699be1bd5e6df8229b08005b534194c1ea92519b09661d71"
     "189b44ac85bbcfbbf8886eb14925c10a6f09f6485b7e3c19503aa44131de2999"
     "b9c002dc827fb75b825da3311935c9f505d48d7ee48f470f0aa7ac5d2a595ab2"
     "90185f1d8362727f2aeac7a3d67d3aec789f55c10bb47dada4eefb2e14aa5d01"
     "b1791a921c4f38cb966c6f78633364ad880ad9cf36eef01c60982c54ec9dd088"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(bm-face ((t (:background "#cba6f7" :foreground "#11111b"))))
 '(bm-persistent-face ((t (:background "#cba6f7" :foreground "#11111b"))))
 '(fill-column-indicator ((t (:foreground "#cba6f7" :style dotted))))
 '(flycheck-error ((t (:underline (:color "#cba6f7" :style line) :weight normal))))
 '(flycheck-info ((t (:underline (:color "#b4befe" :style line) :weight normal))))
 '(flycheck-warning ((t (:underline (:color "#b4befe" :style line) :weight normal))))
 '(font-lock-unused-variable-face ((t (:foreground "#89dceb" :weight bold))))
 '(line-number ((t (:foreground "#cba6f7"))))
 '(line-number-current-line ((t (:foreground "#b4befe" :weight bold))))
 '(treemacs-git-added-face ((t (:foreground "#cba6f7" :weight bold))))
 '(treemacs-git-modified-face ((t (:foreground "#94e2d5" :weight bold))))
 '(treemacs-git-untracked-face ((t (:foreground "#f38ba8" :weight bold)))))
(setq
 python-shell-interpreter "sardine"
 python-shell-interpreter-args "")

(defun sardine/start-sardine ()
  "Start a new interactive Sardine Session"
  (interactive)
  (run-python))

(defun sardine/eval-block ()
  "Evaluate a sardine code block"
  (interactive)
  (mark-paragraph)
  (if (and transient-mark-mode mark-active)
      (python-shell-send-region (point) (mark))
    (python-shell-send-region (point-at-bol) (point-at-eol)))
  (forward-paragraph))

(defun sardine/stop-code ()
  "Stop all the Sardine code currently running"
  (interactive)
  (python-shell-send-string "panic()"))

; Unmapping keys from the Python mode
(add-hook 'python-mode-hook
          (lambda() (local-unset-key (kbd "C-c C-c"))))
(add-hook 'python-mode-hook
          (lambda() (local-unset-key (kbd "C-c C-s"))))
