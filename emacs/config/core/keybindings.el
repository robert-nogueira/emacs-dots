;;; keybindings.el --- Custom keybindings for Emacs
;;; Commentary:
;; This file contains custom keybindings to enhance the usability of Emacs.

;;; Code:

(global-set-key "\M-h" 'help-for-help)
(global-set-key [C-backspace] 'kill-whole-line)
(global-set-key (kbd "C-q") 'other-window)
(global-set-key (kbd "M-i") 'enlarge-window)
(global-set-key (kbd "M-k") 'shrink-window)
(global-set-key (kbd "M-j") 'enlarge-window-horizontally)
(global-set-key (kbd "M-l") 'shrink-window-horizontally)
(global-set-key (kbd "M-<f12>") 'nshell)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-c y") 'company-yasnippet)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)

(provide 'keybindings)
;;; keybindings.el ends here
