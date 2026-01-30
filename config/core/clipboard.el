;;; clipboard.el --- System clipboard integration for Emacs
;;; Commentary:
;; This file configures Emacs to integrate with the system clipboard,
;; supporting both X11 (xclip) and Wayland (wl-clipboard).

;;; Code:

;; Enable clipboard integration
(setq select-enable-clipboard t)
(setq select-enable-primary t)
(setq save-interprogram-paste-before-kill t)

;; Configure clipboard for terminal Emacs
(unless (display-graphic-p)
  (if (executable-find "wl-copy")
      ;; Wayland configuration
      (progn
        (setq interprogram-cut-function
              (lambda (text)
                (start-process "wl-copy" nil "wl-copy" "--type" "text/plain" text)))
        (setq interprogram-paste-function
              (lambda ()
                (when (executable-find "wl-paste")
                  (with-temp-buffer
                    (call-process "wl-paste" nil t nil "--no-newline")
                    (buffer-string))))))
    ;; X11 configuration
    (when (executable-find "xclip")
      (setq interprogram-cut-function
            (lambda (text)
              (start-process "xclip" nil "xclip" "-selection" "clipboard" "-i" text)))
      (setq interprogram-paste-function
            (lambda ()
              (with-temp-buffer
                (call-process "xclip" nil t nil "-selection" "clipboard" "-o")
                (buffer-string)))))))

(provide 'clipboard)
;;; clipboard.el ends here
