;;; docker.el --- Simple Docker integration

;;; Code:

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :ensure t)

(provide 'docker)
;;; docker.el ends here
