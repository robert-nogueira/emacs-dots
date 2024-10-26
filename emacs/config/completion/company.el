;;; company.el --- Configuration for Auto Complete
;;; Commentary:
;; This file configures the Auto Complete package, which provides
;; intelligent completion suggestions and customizes the appearance of completion candidates.

;;; Code:

(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0)
  (setq company-tooltip-animate t)
  (setq company-preview-if-just-one-frontend t)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around 1)
  (setq company-tooltip-align-annotations t))

(use-package company-jedi
  :ensure t
  :after (company python)
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (add-to-list 'company-backends 'company-jedi)
              (company-mode 1))))

(use-package company-quickhelp
  :ensure t)

(company-quickhelp-mode)

(set-face-foreground 'company-tooltip-annotation "#cba6f7")
(set-face-foreground 'company-tooltip-annotation-selection "#11111b")
(set-face-background 'company-tooltip "#11111b")
(set-face-foreground 'company-tooltip "#89b4fa")
(set-face-background 'company-tooltip-selection "#cba6f7")
(set-face-foreground 'company-tooltip-selection "#11111b")
(set-face-foreground 'company-preview "#cba6f7")

(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

(setq pos-tip-foreground-color "#89b4fa"
      pos-tip-background-color "#11111b")

(provide 'company)
;;; company.el ends here
