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

(use-package typescript-mode
  :ensure t)
(setq tags-file-name "~/.dotfiles/emacs/TAGS")


(provide 'web-dev)
;;; web-dev.el ends here
