;;; hooks-dockerfile.el --- Flycheck mode hooks configuration

;;; Commentary:
;; Enable flycheck-inlinde-mode

;;; Code:

(with-eval-after-load 'flycheck
  (add-hook 'python-mode-hook #'flycheck-mode))

(provide 'hooks-flycheck)
;;; hooks-flycheck.el ends here
