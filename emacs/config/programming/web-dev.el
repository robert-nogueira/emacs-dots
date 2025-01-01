;;; web-dev.el --- Web Mode Configuration
;;; Commentary:

;; This file configures Web Mode, enabling auto-pairing,
;; current element highlighting, and autocomplete sources.

;;; Code:

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-markup-indent-offset 2)
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" .  web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.html\\'" . web-mode))
  :commands web-mode)
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

(flycheck-add-mode 'javascript-eslint 'web-mode)

(use-package json-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package prettier-js
  :ensure t
  :hook ((typescript-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))

(use-package lsp-tailwindcss
  :straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             tsx-ts-mode
             js2-mode
             js-ts-mode
             clojure-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

(provide 'web-dev)
;;; web-dev.el ends here
