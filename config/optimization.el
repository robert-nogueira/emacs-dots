;;; optimization.el --- Emacs Performance Tweaks

;;; Code:

;; native compilation (Emacs 28+)
(when (native-comp-available-p)
  (setq native-comp-speed 2)
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-deferred-compilation t)
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache/" user-emacs-directory)))

;; garbage collection optimization
(use-package gcmh
  :defer nil
  :config
  (gcmh-mode 1))

(provide 'optimization)
;;; optimization.el ends here
