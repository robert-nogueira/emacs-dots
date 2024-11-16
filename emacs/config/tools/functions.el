;;; functions.el --- Custom Functions for Emacs
;;; Commentary:

;; This file contains custom functions to enhance Emacs
;; functionality and usability.

;;; Code:

(defun my/ruff-format-buffer ()
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

(defun my/setup-ruff-formatting ()
  "Setup keybindings and hooks for Ruff formatting in Python mode."
  (local-set-key (kbd "C-M-l") 'my/ruff-format-buffer)
  (add-hook 'before-save-hook 'my/ruff-format-buffer nil t))

(defun my/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my/delete-word (- arg)))

(global-set-key (kbd "M-d") 'my/delete-word)
(global-set-key (kbd "<M-backspace>") 'my/backward-delete-word)

(global-set-key (kbd "C-d") 'my/delete-word)
(global-set-key (kbd "<C-backspace>") 'my/backward-delete-word)

(add-hook 'python-mode-hook 'my/setup-ruff-formatting)
(global-set-key (kbd "M-;") 'comment-line)

(provide 'functions)
;;; functions.el ends here
