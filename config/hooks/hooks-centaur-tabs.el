;;; hooks-centaur-tabs.el --- Centaur Tabs hook configuration

;;; Commentary:
;; Enable centaur-tabs-local-mode in dired-mode.

;;; Code:

(with-eval-after-load 'centaur-tabs
  (add-hook 'dired-mode-hook 'centaur-tabs-local-mode))

(provide 'hooks-centaur-tabs)
;;; hooks-centaur-tabs.el ends here
