;;; init.el --- Emacs Initialization File

;;; Commentary:
;; This file loads all necessary configurations for Emacs.

;;; Code:

(let ((nvm-dir (expand-file-name "~/.config/nvm")))
  (setenv "NVM_DIR" nvm-dir)
  (when (file-exists-p (concat nvm-dir "/nvm.sh"))
    (let ((path-from-nvm (shell-command-to-string
                          (concat
                           "bash -c 'source " nvm-dir "/nvm.sh && echo $PATH'"))))
      (setenv "PATH" (string-trim path-from-nvm))
      (setq exec-path (split-string (string-trim path-from-nvm) path-separator)))))

(defvar config-dir (expand-file-name "config" user-emacs-directory))
(setq shell-file-name "/bin/zsh")
(setq explicit-shell-file-name "/bin/zsh")

(add-to-list 'load-path config-dir)

(load (expand-file-name "core/packages" config-dir))
(load (expand-file-name "optimization" config-dir))
(load (expand-file-name "core/keybindings" config-dir))
(load (expand-file-name "core/settings" config-dir))
(load (expand-file-name "tools/functions" config-dir))

(load (expand-file-name "completion/company" config-dir))
(load (expand-file-name "completion/vertico" config-dir))
(load (expand-file-name "completion/marginalia" config-dir))
(load (expand-file-name "completion/orderless" config-dir))
(load (expand-file-name "completion/consult" config-dir))

(load (expand-file-name "ui/interface" config-dir))
(load (expand-file-name "ui/theme" config-dir))
(load (expand-file-name "ui/faces" config-dir))
(load (expand-file-name "ui/dashboard" config-dir))
(load (expand-file-name "ui/modeline" config-dir))

(load (expand-file-name "programming/lsp" config-dir))
(load (expand-file-name "programming/python" config-dir))
(load (expand-file-name "programming/rust" config-dir))
(load (expand-file-name "programming/web-dev" config-dir))

(load (expand-file-name "tools/treemacs" config-dir))
(load (expand-file-name "tools/centaur-tabs" config-dir))
(load (expand-file-name "tools/flycheck" config-dir))
(load (expand-file-name "tools/projectile" config-dir))
(load (expand-file-name "tools/vterm" config-dir))
(load (expand-file-name "tools/discord" config-dir))
(load (expand-file-name "tools/vc" config-dir))
(load (expand-file-name "tools/docker" config-dir))
(load (expand-file-name "tools/ligature" config-dir))
(load (expand-file-name "tools/terraform" config-dir))

;; load hooks and subdirs

(add-to-list 'load-path (expand-file-name "hooks" config-dir))

;; load all hooks files in hooks root
(dolist (hook-file
         (directory-files (expand-file-name "hooks" config-dir) t "^[^.#].*\\.el$"))
  (load hook-file))

;; load lsp hooks
(let ((lsp-hooks-dir (expand-file-name "hooks/lsp" config-dir)))
  (when (file-directory-p lsp-hooks-dir)
    (add-to-list 'load-path lsp-hooks-dir)
    (dolist (file (directory-files lsp-hooks-dir t "^[^.#].*\\.el$"))
      (load file))))

;; load python hooks
(let ((python-hooks-dir (expand-file-name "hooks/python" config-dir)))
  (when (file-directory-p python-hooks-dir)
    (add-to-list 'load-path python-hooks-dir)
    (dolist (file (directory-files python-hooks-dir t "^[^.#].*\\.el$"))
      (load file))))

(load (expand-file-name "misc" config-dir))
(load (expand-file-name "aliases" config-dir))

(custom-set-variables
 '(custom-safe-themes
   '("8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a"
     default)))

(custom-set-faces
 '(default ((t (:background nil :height 120))))
 '(doom-modeline-buffer-file ((t (:foreground "#cba6f7"))))
 '(doom-modeline-buffer-major-mode ((t (:foreground "#cba6f7"))))
 '(doom-modeline-buffer-minor-mode ((t (:foreground "#cba6f7"))))
 '(doom-modeline-buffer-modified ((t (:foreground "#cba6f7"))))
 '(doom-modeline-error ((t (:foreground "#cba6f7"))))
 '(doom-modeline-info ((t (:foreground "#cba6f7"))))
 '(doom-modeline-lsp-success ((t (:foreground "#cba6f7"))))
 '(doom-modeline-urgent ((t (:foreground "#cba6f7"))))
 '(doom-modeline-warning ((t (:foreground "#cba6f7"))))
 '(fill-column-indicator ((t (:foreground "#cba6f7" :style dotted))))
 '(flycheck-error ((t (:underline (:color "#cba6f7" :style line) :weight normal))))
 '(flycheck-info ((t (:underline (:color "#b4befe" :style line) :weight normal))))
 '(flycheck-warning ((t (:underline (:color "#b4befe" :style line) :weight normal))))
 '(font-lock-unused-variable-face ((t (:foreground "#89dceb" :weight bold))))
 '(line-number ((t (:foreground "#cba6f7"))))
 '(line-number-current-line ((t (:foreground "#b4befe" :weight bold))))
 '(treemacs-directory-face ((t (:foreground "#89b4fa"))))
 '(treemacs-git-added-face ((t (:foreground "#cba6f7" :weight bold))))
 '(treemacs-git-modified-face ((t (:foreground "#94e2d5" :weight bold))))
 '(treemacs-git-untracked-face ((t (:foreground "#f38ba8" :weight bold))))
 '(treemacs-nerd-icons-file-face ((t (:foreground "#89b4fa")))))

(custom-set-faces
 '(default ((t (:background nil)))))

;;; init.el ends here
