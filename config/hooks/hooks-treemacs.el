;;; hooks-treemacs.el --- Treemacs hook configuration

;;; Commentary:
;; Disable line numbers in treemacs-mode.

;;; Code:

(with-eval-after-load 'treemacs
  (defun my/treemacs-disable-line-numbers ()
    (display-line-numbers-mode -1))

  (add-hook 'treemacs-mode-hook #'my/treemacs-disable-line-numbers))

(provide 'hooks-treemacs)
;;; hooks-treemacs.el ends here
