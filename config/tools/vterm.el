;;; vterm.el --- Configuration for vterm and multi-vterm
;;; Commentary:
;; Configuration for vterm, vterm-toggle, and multi-vterm with unified keybindings

;;; Code:

(use-package vterm
  :ensure t
  :defer nil
  :config
  (setq vterm-kill-buffer-on-exit t))

(use-package multi-vterm
  :ensure t
  :after vterm
  :bind (("C-c t" . multi-vterm-project)))

(use-package vterm-toggle
  :ensure t
  :bind (("C-c j" . vterm-toggle-forward)
         ("C-c l" . vterm-toggle-backward)))

(provide 'vterm)
;;; vterm.el ends here
