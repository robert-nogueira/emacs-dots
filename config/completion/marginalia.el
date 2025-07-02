;;; marginalia.el --- Configuration for Marginalia
;;; Commentary:
;; This file configures the Marginalia package, which provides
;; rich annotations in the minibuffer to enhance the completion experience.

;;; Code:

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-lv))
  :init
  (marginalia-mode))

(provide 'marginalia)
;;; marginalia.el ends here
