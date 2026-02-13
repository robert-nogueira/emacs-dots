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

(defun on-frame-created (frame)
  "Set default face background for FRAME if not graphic."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))

(add-hook 'after-make-frame-functions #'on-frame-created)

(add-hook 'window-setup-hook 'on-after-init)
(set-face-attribute 'line-number nil :background 'unspecified)
(set-face-attribute 'line-number-current-line nil :background 'unspecified)
