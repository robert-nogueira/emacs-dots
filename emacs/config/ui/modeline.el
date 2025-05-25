;;; modeline.el --- Doom Modeline Configuration
;;; Commentary:
;; Configures doom-modeline with custom height, file name style,
;; major mode icons, word count, LSP info, and other preferences.

;;; Code:

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(setq doom-modeline-height 30)
(setq doom-modeline-bar-width 2)
(setq doom-modeline-buffer-file-name-style 'file-name)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-position-line-format '("%l"))
(setq doom-modeline-total-line-number t)
(setq doom-modeline-enable-word-count t)
(setq doom-modeline-project-name t)
(setq doom-modeline-lsp t)
(setq doom-modeline-time-icon nil)
(setq doom-modeline-modal t)
(setq doom-modeline-time nil)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-lsp t)

(provide 'modeline)
;;; modeline.el ends here
