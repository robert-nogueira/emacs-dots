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

(defun ruff-format-buffer ()
  "Format the buffer with ruff and restore the cursor position."
  (interactive)
  (let ((saved-point (point)))
    (let ((exit-code (call-process-region (point-min) (point-max)
                                          "ruff" nil "*Ruff Format Output*"
                                          t "format" "-")))
      (if (zerop exit-code)
          (progn
            (shell-command-on-region (point-min) (point-max) "ruff format -"
                                     nil t)
            (goto-char saved-point)
            (message "Buffer formatted and saved with ruff!"))
        (message "Error formatting with ruff! Exit code: %d" exit-code)))))

(defun setup-ruff-formatting ()
  "Setup keybindings and hooks for Ruff formatting in Python mode."
  (local-set-key (kbd "C-M-l") 'ruff-format-buffer)
  (add-hook 'before-save-hook 'ruff-format-buffer nil t))

(add-hook 'python-mode-hook 'setup-ruff-formatting)

(global-set-key (kbd "M-;") 'comment-line)

(provide 'functions)
;;; functions.el ends here
