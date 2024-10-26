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
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev)))))

(provide 'web-dev)
;;; web-dev.el ends here
