;;; packages.el --- Emacs Packages Setup
;;; Commentary:
;; Package management using straight.el only

;;; Code:

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install and configure use-package via straight
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Global use-package defaults
(setq use-package-always-defer t)

;; Auto package updates (managed by straight)
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "21:00"))

;; Native compilation tuning
(when (native-comp-available-p)
  (setq native-comp-speed 2)
  (setq native-comp-async-report-warnings-errors nil)
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache/" user-emacs-directory)))

(provide 'packages)
;;; packages.el ends here
