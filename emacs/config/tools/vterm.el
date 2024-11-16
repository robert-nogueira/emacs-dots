;;; vterm.el --- Configuration for vterm and vterm-toggle
;;; Commentary:
;; This file configures vterm, a terminal emulator in Emacs,
;; and vterm-toggle for easier terminal management.

;;; Code:

(use-package vterm
  :ensure t
  :config
  (setq vterm-kill-buffer-on-exit t)
  (global-set-key (kbd "C-c C-t") 'vterm)
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0))))

(use-package vterm-toggle
  :ensure t
  :bind (("M-<f12>" . vterm-toggle)
         ("C-c j" . vterm-toggle-forward)
         ("C-c l" . vterm-toggle-backward)))

(provide 'vterm)
;;; vterm.el ends here
