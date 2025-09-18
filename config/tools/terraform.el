;;; terraform.el --- Configuration for Terraform
;;; Commentary:
;; This file configures Terraform mode in Emacs.
;; It provides syntax highlighting, indentation, and hooks
;; to work efficiently with Terraform configuration files.

;;; Code:

(use-package terraform-mode
  :ensure t
  :custom
  (terraform-indent-level 4) ;; Sets indentation level to 4 spaces
  :config
  (defun my-terraform-mode-init ()
    "Custom initialization for Terraform mode.
You can enable additional minor modes here, such as outline-minor-mode."
    ;; Uncomment the following line if you want to use outline-minor-mode
    ;; (outline-minor-mode 1)
    )

  ;; Add custom init function to terraform-mode-hook
  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

(provide 'terraform)
;;; terraform.el ends here
