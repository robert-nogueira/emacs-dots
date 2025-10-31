;;; flycheck.el --- Flycheck Configuration for Python
;;; Commentary:

;; Configure Flycheck with Ruff and Mypy, ensuring error display works immediately.
;; Adds prettier inline error display using flycheck-inline.

;;; Code:

(use-package flycheck
  :ensure t
  :defer nil
  :init
  (setq flycheck-check-syntax-automatically
        '(save mode-enabled idle-change))
  (setq flycheck-idle-change-delay 0)
  (setq flycheck-checker-cache "~/.flycheck-cache")
  (setq flycheck-indication-mode nil))

(defun flycheck-display-error-messages-unless-error-buffer (errors)
  (unless (get-buffer-window flycheck-error-list-buffer)
    (flycheck-display-error-messages errors)))

(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-buffer)

(provide 'flycheck)
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
;;; flycheck.el ends here
