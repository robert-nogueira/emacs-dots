;;; orderless.el --- Configuration for Orderless
;;; Commentary:
;; This file configures the Orderless package, which provides a flexible completion
;; style for improved completion capabilities.

;;; Code:

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

(provide 'orderless)
;;; orderless.el ends here
