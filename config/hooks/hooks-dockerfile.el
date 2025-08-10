;;; hooks-dockerfile.el --- Dockerfile mode hooks configuration

;;; Commentary:
;; Disable aggressive-indent-mode in dockerfile-mode.

;;; Code:

(with-eval-after-load 'dockerfile-mode
  (defun my/dockerfile-disable-aggressive-indent ()
    (aggressive-indent-mode -1))

  (add-hook 'dockerfile-mode-hook #'my/dockerfile-disable-aggressive-indent))

(provide 'hooks-dockerfile)
;;; hooks-dockerfile.el ends here
