;;; centaur-tabs.el --- Configuration for Centaur Tabs
;;; Commentary:

;; This file configures Centaur Tabs, enabling a tab-based navigation
;; system for Emacs, along with some customizations for appearance and behavior.

;;; Code:

(use-package centaur-tabs
  :demand
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-icon-type 'nerd-icons)
  (setq centaur-tabs-set-bar 'under)
  (setq x-underline-at-descent-line t)
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-group-by-projectile-project)
  (setq centaur-theme 'system)
  (setq centaur-tabs-style "slant")
  (setq centaur-tabs-modified-marker "👀")
  :bind
  ("C-<iso-lefttab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward)
  ("C-s-n" . centaur-tabs--create-new-tab)
  ("C-s-w" . centaur-tabs--kill-this-buffer-dont-ask)
  ("C-s-S-W" . centaur-tabs-kill-unmodified-buffers-in-current-group))

(provide 'centaur-tabs)
;;; centaur-tabs.el ends here
(add-hook 'dired-mode-hook 'centaur-tabs-local-mode)
