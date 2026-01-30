;;; keybindings.el --- Custom keybindings for Emacs
;;; Commentary:
;; This file contains custom keybindings to enhance the usability of Emacs.

;;; Code:

(global-set-key "\M-h" 'help-for-help)
(global-set-key (kbd "C-q") 'other-window)
(global-set-key (kbd "C-k") 'enlarge-window)
(global-set-key (kbd "C-i") 'shrink-window)
(global-set-key (kbd "C-l") 'enlarge-window-horizontally)
(global-set-key (kbd "C-j") 'shrink-window-horizontally)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-c y") 'company-yasnippet)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)
(global-set-key (kbd "C-c h") 'hs-hide-block)
(global-set-key (kbd "C-c s") 'hs-show-block)
(global-set-key (kbd "C-c C-r") 'lsp-rename)
(global-set-key (kbd "C-c C-l") #'display-line-numbers-mode)

(define-key input-decode-map "\e[127;6u" [C-S-backspace])
(global-set-key (kbd "C-S-<backspace>") 'kill-whole-line)

(with-eval-after-load 'lsp-ui
  (defun toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-hide)
          (lsp-ui-doc-mode -1))
      (progn
        (lsp-ui-doc-mode 1)
        (lsp-ui-doc-show))))

  (global-set-key (kbd "M-p") #'toggle-lsp-ui-doc))

(provide 'keybindings)
;;; keybindings.el ends here
