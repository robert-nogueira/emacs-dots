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
  :bind (("C-c t" . my/vterm-toggle-project-root-or-default)
         ("C-c j" . vterm-toggle-forward)
         ("C-c l" . vterm-toggle-backward)))

(defun my/vterm-toggle-project-root-or-default ()
  (interactive)
  (let ((default-directory
         (or (and (fboundp 'projectile-project-root)
                  (projectile-project-root))
             default-directory)))
    (vterm-toggle)))


(provide 'vterm)
;;; vterm.el ends here
