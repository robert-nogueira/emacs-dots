;;; hooks-vterm.el --- Vterm hook configuration

;;; Commentary:
;; Disable line numbers in vterm-mode.

;;; Code:

(with-eval-after-load 'vterm
  (defun my/vterm-disable-line-numbers ()
    (display-line-numbers-mode -1))

  (add-hook 'vterm-mode-hook #'my/vterm-disable-line-numbers))

(provide 'hooks-vterm)
;;; hooks-vterm.el ends here
