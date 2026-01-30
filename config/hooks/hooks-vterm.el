;;; hooks-vterm.el --- Vterm hook configuration
;;; Commentary:
;; Disable line numbers and mode line in vterm-mode.
;;; Code:

(with-eval-after-load 'vterm
  (defun my/vterm-cleanup ()
    "Disable line numbers, mode line and centaur-tabs in vterm buffer."
    (display-line-numbers-mode -1)
    (setq-local mode-line-format nil))

  (add-hook 'vterm-mode-hook #'my/vterm-cleanup))

(provide 'hooks-vterm)
;;; hooks-vterm.el ends here
