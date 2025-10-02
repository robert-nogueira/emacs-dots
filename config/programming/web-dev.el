(use-package web-mode
  :defer nil
  :mode ("\\.html?\\'" "\\.tsx\\'" "\\.jsx\\'")
  :config
  (setq web-mode-enable-auto-quoting nil
        web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-attr-value-indent-offset 2))

;; (use-package rjsx-mode
;;   :defer nil
;;   :mode ("\\.js\\'" "\\.jsx\\'"))

(use-package typescript-mode
  :defer nil
  :mode ("\\.ts\\'" "\\.tsx\\'"))

(use-package tide
  :defer nil
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (tsx-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (tsx-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :config
  (setq tide-format-options
        '(:indentSize 2 :tabSize 2 :insertSpaceAfterCommaDelimiter t)))

(use-package lsp-tailwindcss
  :defer nil
  :after lsp-mode)

(use-package company
  :defer nil
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1))

(use-package flycheck
  :defer nil
  :hook (after-init . global-flycheck-mode))

(use-package prettier-js
  :defer nil
  :hook ((typescript-mode . prettier-js-mode)
         (tsx-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))

(use-package yasnippet
  :defer nil
  :hook (after-init . yas-global-mode))

(use-package emmet-mode
  :defer nil
  :hook ((web-mode css-mode) . emmet-mode)
  :config
  (setq emmet-expand-jsx-className? t))
