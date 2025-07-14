;;; functions.el --- Custom Functions for Emacs
;;; Commentary:
;;
;; This file provides a collection of custom functions to enhance Emacs
;; functionality and usability.  It includes:
;;
;; - **Python Formatting**: Functions for formatting Python code using Ruff.
;; - **Word Deletion**: Custom keybindings for deleting words without adding
;;   them to the kill ring.
;; - **Sardine Integration**: Functions for interacting with Sardine, a live
;;   coding environment for Python.  This includes starting a Sardine shell,
;;   evaluating code blocks, and stopping execution.
;;
;; The Sardine-related functions allow users to run Python code interactively
;; in a dedicated buffer while disabling Flycheck to avoid conflicts.
;; Users can send code to the Sardine process and interrupt execution if needed.

;;; Code:

(defun my/ruff-format-buffer ()
  "Format the buffer with ruff and restore the cursor position.
Only runs in python-mode."
  (interactive)
  (if (derived-mode-p 'python-mode)  ; Check if in python-mode
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
            (message "Error formatting with ruff! Exit code: %d" exit-code))))
    (message "Not in Python mode. Ruff formatting skipped.")))

(defun my/setup-ruff-formatting ()
  "Setup keybindings and hooks for Ruff formatting in Python mode."
  (local-set-key (kbd "C-M-l") 'my/ruff-format-buffer)
  (add-hook 'before-save-hook 'my/ruff-format-buffer nil t))  ; Hook only for current buffer

(defun my/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to kill-ring'."
  (interactive "p")
  (my/delete-word (- arg)))

(global-set-key (kbd "M-d") 'my/delete-word)
(global-set-key (kbd "<M-backspace>") 'my/backward-delete-word)
(global-set-key (kbd "C-d") 'my/delete-word)
(global-set-key (kbd "<C-backspace>") 'my/backward-delete-word)
(global-set-key (kbd "M-;") 'comment-line)

(add-hook 'python-mode-hook 'my/setup-ruff-formatting)

(defun sardine/activate ()
  "Start a Sardine shell session and disable Python formatting and Flycheck."
  (interactive)
  (let ((prev-buffer (current-buffer))
        (shell-buffer (get-buffer-create "*Sardine*")))
    (setq
     python-shell-interpreter "sardine"
     python-shell-interpreter-args "")
    (apply 'make-comint-in-buffer "Sardine" shell-buffer "sardine" nil nil)
    (pop-to-buffer shell-buffer)
    (with-current-buffer prev-buffer
      (when (bound-and-true-p flycheck-mode)
        (flycheck-mode -1))
      (remove-hook 'before-save-hook 'my/ruff-format-buffer t))
    (pop-to-buffer prev-buffer)
    (message "Sardine activated: Formatting and Flycheck disabled.")))

(defun sardine/deactivate ()
  "Reactivate Python formatting and Flycheck after using Sardine."
  (interactive)
  (with-current-buffer (current-buffer)
    (when (derived-mode-p 'python-mode)
      (flycheck-mode 1)
      (add-hook 'before-save-hook 'my/ruff-format-buffer nil t))
    (message "Sardine deactivated: Formatting and Flycheck re-enabled.")))

(defun sardine/eval-block ()
  "Evaluate a selected region in Sardine.
If no region is selected, send the entire buffer."
  (interactive)
  (let ((process (get-buffer-process "*Sardine*")))
    (if process
        (progn
          (if (use-region-p)
              (comint-send-region process (region-beginning) (region-end))
            (comint-send-region process (point-min) (point-max)))
          (comint-send-string process "\n")
          (deactivate-mark))
      (error "No Sardine process running"))))

(defun sardine/stop-code ()
  "Stop all the Sardine code currently running."
  (interactive)
  (let ((process (get-buffer-process "*Sardine*")))
    (if process
        (progn
          (comint-send-string process "panic()\n")
          (message "Sardine code stopped"))
      (error "No Sardine process running"))))

(global-set-key (kbd "C-<return>") #'sardine/eval-block)
(global-set-key (kbd "C-.") #'sardine/stop-code)

(defun my/kill-region-or-line ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line)))

(global-set-key (kbd "C-w") #'my/kill-region-or-line)

(provide 'functions)
;;; functions.el ends here
