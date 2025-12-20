;;; hooks-python.el --- Python mode hook configuration

;;; Commentary:
;; Setup ligature, company backend, line numbers, and font-lock.

;;; Code:

(with-eval-after-load 'python
  (defun my/python-mode-setup ()
    (when (featurep 'ligature)
      (ligature-set-ligatures 'python-mode '("->" "=>" "==" "!=" ">=" "<="))
      (ligature-mode 1))

    (setq-local company-backends '(company-capf company-dabbrev))

    ;; (display-line-numbers-mode 1)
    (font-lock-mode 1))

  (add-hook 'python-mode-hook #'my/python-mode-setup))

(provide 'hooks-python)
;;; hooks-python.el ends here
