;;; neotree.el --- Configuration for Neotree
;;; Commentary:
;; This file configures Neotree, a file and directory visualization tool
;; in Emacs.  It allows for easy navigation of your project's file structure.

;;; Code:

(use-package neotree
  :ensure t
  :bind (("M-\\" . neotree-toggle))
  :config
  (setq neo-theme 'icons)
  (setq neo-window-fixed-size nil)
  (setq neo-smart-open t)
  (setq delete-by-moving-to-trash t)
  (setq neo-confirm-delete-file nil)
  (setq neo-confirm-create-file nil))

(provide 'neotree)
;;; neotree.el ends here
