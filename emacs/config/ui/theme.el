;;; theme.el --- Configuration for Themes
;;; Commentary:
;; This file configures the Rose Pine and Catppuccin themes for Emacs.

;;; Code:

(use-package catppuccin-theme :ensure t)
(use-package autothemer :ensure t)

;;; Install theme from GitHub
(straight-use-package
 '(rose-pine-emacs :host github :repo "thongpv87/rose-pine-emacs" :branch "master"))
(load-theme 'rose-pine-moon t)


(provide 'theme)
;;; theme.el ends here
