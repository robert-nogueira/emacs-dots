;;; functions.el --- Custom Functions for Emacs
;;; Commentary:

;; This file contains custom functions to enhance Emacs
;; functionality and usability.

;;; Code:

(defun nshell ()
  "Create a shell in another window vertically."
  (interactive)
  (let ((buf (shell)))
    (switch-to-buffer (other-buffer buf))
    (let ((win (split-window-below)))
      (set-window-buffer win buf)
      (select-window win))))

(defun format-on-save ()
  "Run formatter on save for Python files, skipping temporary files."
  (when (and (eq major-mode 'python-mode)
             (buffer-file-name)
             (not (string-match-p "\\.#.*" (buffer-file-name)))) ;; Ignora arquivos temporários
    (shell-command "echo antes && poetry run task format && echo teste")
    (revert-buffer :ignore-auto :noconfirm)))
(add-hook 'after-save-hook 'format-on-save)

(provide 'functions)
;;; functions.el ends here
