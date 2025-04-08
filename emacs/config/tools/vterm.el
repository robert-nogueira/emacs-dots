;;; vterm.el --- Configuration for vterm and vterm-toggle
;;; Commentary:
;; This file configures vterm, a terminal emulator in Emacs,
;; and vterm-toggle for easier terminal management.

;;; Code:

(use-package vterm
  :ensure t
  :config
  (setq vterm-kill-buffer-on-exit t)
  (global-set-key (kbd "C-c C-t") 'vterm))

(use-package vterm-toggle
  :ensure t
  :bind (("C-c t" . vterm-toggle)
         ("C-c j" . vterm-toggle-forward)
         ("C-c l" . vterm-toggle-backward)))

(provide 'vterm)
;;; vterm.el ends here
