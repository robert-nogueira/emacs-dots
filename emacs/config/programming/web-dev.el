;;; web-dev.el --- Web Mode Configuration

;;; Code:

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t)
  :hook (web-mode . (lambda ()
                      (when (string-equal "tsx" (file-name-extension buffer-file-name))
                        (tide-setup)
                        (tide-hl-identifier-mode)
                        (flycheck-mode)
                        (eldoc-mode)
                        (company-mode)))))

(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2)
  :hook ((typescript-mode . subword-mode)
         (typescript-mode . flycheck-mode)))

(use-package json-mode
  :ensure t)

(use-package prettier-js
  :ensure t
  :hook ((typescript-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package lsp-tailwindcss
  :straight '(lsp-tailwindcss
              :type git
              :host github
              :repo "merrickluo/lsp-tailwindcss")
  :after lsp-mode
  :config
  (setq lsp-tailwindcss-add-on-mode t
        lsp-tailwindcss-experimental-class-regex
        ["tw([^]*)"
         "tw=\"([^\"]*)"
         "tw={\"([^\"}]*)"
         "tw\\.\\w+([^]*)"
         "tw\\(.*?\\)([^]*)"])
  (dolist (mode '(css-mode css-ts-mode typescript-mode typescript-ts-mode
			   tsx-ts-mode js2-mode js-ts-mode clojure-mode))
    (add-to-list 'lsp-tailwindcss-major-modes mode)))

(use-package prettier
  :ensure t)

(add-hook 'after-init-hook #'global-prettier-mode)

(use-package emmet-mode
  :ensure t
  :hook ((html-mode css-mode web-mode typescript-tsx-mode) . emmet-mode)
  :config
  (setq emmet-expand-jsx-className? t))

(provide 'web-dev)

;;; web-dev.el ends here
