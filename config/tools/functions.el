;;; functions.el --- Custom Functions for Emacs
;;; Commentary:
;;
;; This file provides a collection of custom functions to enhance Emacs
;; functionality and usability. It includes:
;;
;; - Word deletion helpers.
;; - Sardine integration for live Python coding.
;;
;;; Code:

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

(defun sardine/activate ()
  "Start a Sardine shell session and disable Python formatting and Flycheck."
  (interactive)
  (let ((prev-buffer (current-buffer))
        (shell-buffer (get-buffer-create "*Sardine*")))
    (setq
     python-shell-interpreter "sardine"
     python-shell-interpreter-args "")
    (apply 'make-comint-in-buffer
           "Sardine"
           shell-buffer
           "sardine"
           nil
           nil)
    (pop-to-buffer shell-buffer)
    (with-current-buffer prev-buffer
      (when (bound-and-true-p flycheck-mode)
        (flycheck-mode -1))
      (when (bound-and-true-p apheleia-mode)
        (apheleia-mode -1)))
    (pop-to-buffer prev-buffer)
    (message "Sardine activated: Formatting and Flycheck disabled.")))

(defun sardine/deactivate ()
  "Reactivate Python formatting and Flycheck after using Sardine."
  (interactive)
  (with-current-buffer (current-buffer)
    (when (derived-mode-p 'python-mode)
      (flycheck-mode 1)
      (apheleia-mode 1))
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

(global-set-key (kbd "C-<return>") 'sardine/eval-block)
(global-set-key (kbd "C-.") 'sardine/stop-code)

(defun my/kill-region-or-line ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line)))

(global-set-key (kbd "C-w") 'my/kill-region-or-line)

(defun my/copy-region-or-line ()
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save
     (line-beginning-position)
     (line-beginning-position 2))))

(global-set-key (kbd "M-w") 'my/copy-region-or-line)

(provide 'functions)
;;; functions.el ends here
