(setq package-enable-at-startup nil)
(setq gc-cons-threshold most-positive-fixnum)
(setq read-process-output-max (* 1024 1024))

(setq default-frame-alist
      '((background-color . "#212337")
        (foreground-color . "#c0caf5")
        (cursor-color     . "#c0caf5")))

(add-to-list 'initial-frame-alist '(background-color . "#212337"))
(add-to-list 'initial-frame-alist '(foreground-color . "#c0caf5"))

(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)
