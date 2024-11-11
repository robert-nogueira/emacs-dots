;;; company.el --- Configuration for Auto Complete
;;; Commentary:
;; This file configures the Auto Complete package, which provides
;; intelligent completion suggestions and customizes the appearance of completion candidates.

;;; Code:

(use-package company
  :ensure t
  :config
  (setq company-require-match nil)
  (setq company-backends '((company-capf company-dabbrev company-files company-keywords)))

  (global-company-mode 1)
  (setq company-idle-delay 0)
  (setq company-tooltip-animate t)
  (setq company-preview-if-just-one-frontend t)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t))

(add-hook 'python-mode-hook 'company-mode)

(global-set-key (kbd "s-SPC") 'company-complete)

(use-package company-quickhelp
  :ensure t)

(company-quickhelp-mode)

(set-face-foreground 'company-tooltip-annotation "#cba6f7")
(set-face-foreground 'company-tooltip-annotation-selection "#11111b")
(set-face-background 'company-tooltip "#11111b")
(set-face-foreground 'company-tooltip "#89b4fa")
(set-face-background 'company-tooltip-selection "#cba6f7")
(set-face-foreground 'company-tooltip-selection "#11111b")
(set-face-background 'company-preview "#11111b")
(set-face-foreground 'company-preview "#cba6f7")

(setq pos-tip-foreground-color "#89b4fa"
      pos-tip-background-color "#11111b")

(define-key company-active-map [escape] 'company-abort)

(provide 'company)
;;; company.el ends here
