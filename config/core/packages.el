;;; packages.el --- Emacs Packages Setup
;;; Commentary:
;; Package configuration using straight.el + use-package
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

;; Use straight by default with use-package
(setq straight-use-package-by-default t)

;; Integrate use-package with straight.el
(straight-use-package 'use-package)

;; Automatic package updates
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "21:00"))

;; General use-package settings
(setq use-package-always-defer t)
(setq use-package-always-ensure t)

;; Native compilation settings
(when (native-comp-available-p)
  (setq native-comp-speed 2
        native-comp-async-report-warnings-errors nil)
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache/" user-emacs-directory)))

(provide 'packages)
;;; packages.el ends here
