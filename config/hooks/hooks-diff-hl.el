;;; hooks-diff-hl.el --- Diff-hl hooks configuration

;;; Commentary:
;; Configura hooks para diff-hl e integração com magit

;;; Code:

(with-eval-after-load 'diff-hl
  (add-hook 'after-save-hook #'diff-hl-update)

  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(provide 'hooks-diff-hl)
;;; hooks-diff-hl.el ends here
