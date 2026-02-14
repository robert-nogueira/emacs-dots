;;; centaur-tabs.el --- Configuration for Centaur Tabs
;;; Commentary:
;; Centaur Tabs + fixes for the "line" background in TUI frames (daemon),
;; applied per frame and avoiding errors if faces/functions do not exist.
;;; Code:

(use-package centaur-tabs
  :demand t
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-set-icons t
        centaur-tabs-icon-type 'nerd-icons
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t
        centaur-tabs-set-close-button nil
        centaur-tabs-set-modified-marker t
        centaur-tabs-cycle-scope 'tabs
        centaur-theme 'system
        centaur-tabs-style "slant"
        centaur-tabs-modified-marker "👀")
  (centaur-tabs-group-by-projectile-project)

  ;; Binds
  :bind
  ("C-<iso-lefttab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward)
  ("C-s-n" . centaur-tabs--create-new-tab)
  ("C-s-w" . centaur-tabs--kill-this-buffer-dont-ask)
  ("C-s-S-W" . centaur-tabs-kill-unmodified-buffers-in-current-group))

;; Buffer groups
(defun my-centaur-tabs-buffer-groups ()
  (list
   (cond
    ((derived-mode-p 'vterm-mode) "VTerm")
    ((buffer-file-name) "Files")
    (t "Other"))))
(setq centaur-tabs-buffer-groups-function #'my-centaur-tabs-buffer-groups)

;; Note for me: function to fix the "line" background per frame (TUI/daemon safe)
(defun my/centaur-tabs-fix-line-for-frame (frame)
  "Adjust header-line/tab-line and related faces for FRAME.
If the `default` background is a string, use it; otherwise use nil
to avoid the 'strange line' in TUI."
  (with-selected-frame frame
    ;; Note for me: get default background safely
    (let* ((maybe-bg (face-background 'default frame))
           (bg (if (and maybe-bg (stringp maybe-bg)) maybe-bg nil))
           (fg "#cba6f7")
           (unfg "#89b4fa")
           (modfg "#f38ba8"))
      ;; Note for me: set header-line/tab-line to avoid strange line
      (set-face-attribute 'header-line frame :background bg :foreground fg)
      (set-face-attribute 'tab-line frame :background bg :foreground fg)
      ;; Note for me: adjust centaur-tabs faces if they exist
      (when (facep 'centaur-tabs-selected)
        (set-face-attribute 'centaur-tabs-selected frame
                            :background bg :foreground fg :weight 'bold))
      (when (facep 'centaur-tabs-unselected)
        (set-face-attribute 'centaur-tabs-unselected frame
                            :background bg :foreground unfg))
      (when (facep 'centaur-tabs-selected-modified)
        (set-face-attribute 'centaur-tabs-selected-modified frame
                            :background bg :foreground modfg))
      (when (facep 'centaur-tabs-unselected-modified)
        (set-face-attribute 'centaur-tabs-unselected-modified frame
                            :background bg :foreground modfg))
      ;; Note for me: update centaur-tabs if available
      (when (fboundp 'centaur-tabs-update)
        (centaur-tabs-update)))))

;; Apply to the current frame safely
(when (framep (selected-frame))
  (my/centaur-tabs-fix-line-for-frame (selected-frame)))

;; Apply to all future frames (includes emacsclient -t / -c)
(add-hook 'after-make-frame-functions #'my/centaur-tabs-fix-line-for-frame)

;; Note for me: ensure fixes are applied if the package loads later
(with-eval-after-load 'centaur-tabs
  (my/centaur-tabs-fix-line-for-frame (selected-frame))
  (when (fboundp 'centaur-tabs-update)
    (centaur-tabs-update)))

(provide 'centaur-tabs)
;;; centaur-tabs.el ends here
