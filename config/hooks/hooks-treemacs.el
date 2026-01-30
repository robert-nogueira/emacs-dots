;;; hooks-treemacs.el --- Treemacs hook configuration
;;; Commentary:
;; Disable line numbers and the mode line only in treemacs-mode.
;;; Code:

(with-eval-after-load 'treemacs
  (defun my/treemacs-cleanup ()
    "Disable line numbers and hide the mode line only in Treemacs buffer."
    (when (eq major-mode 'treemacs-mode)
      (display-line-numbers-mode -1)
      (setq mode-line-format nil)))

  (add-hook 'treemacs-mode-hook #'my/treemacs-cleanup)

  ;; Garantir que aconteça após todas as inicializações
  (add-hook 'after-change-major-mode-hook #'my/treemacs-cleanup))

(provide 'hooks-treemacs)
;;; hooks-treemacs.el ends here
