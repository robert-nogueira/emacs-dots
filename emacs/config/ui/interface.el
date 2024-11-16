;;; interface.el --- User Interface Settings
;;; Commentary:

;; This file contains configurations for the Emacs user interface
;; to enhance usability and aesthetics.

;;; Code:

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq-default cursor-type 'bar)
(global-display-line-numbers-mode 1)
(setq select-enable-clipboard t)
(global-hl-line-mode 1)
(delete-selection-mode t)
(electric-pair-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(setq-default display-fill-column-indicator-column 79)
(setq use-dialog-box nil)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(display-time-mode 1)

;;invisible cursor
(setq x-pointer-shape x-pointer-invisible)
(setq x-sensitive-text-pointer-shape x-pointer-invisible)
(setq void-text-area-pointer 'text)
(set-mouse-color (cdr (assoc 'mouse-color (frame-parameters))))

(provide 'interface)
;;; interface.el ends here
