;;; faces.el --- Custom face settings for a personalized Emacs UI
;;; Commentary:

;; Define visual styles to match the desired theme and improve readability.

;;; Code:

(set-face-foreground 'vertical-border "#cba6f7")
(set-face-attribute 'line-number nil :height 0.7)
(set-face-attribute 'line-number-current-line nil :height 0.7)

(custom-set-faces
 '(default ((t (:background nil :height 120))))
 '(fill-column-indicator ((t (:foreground "#cba6f7" :style dotted))))
 '(line-number ((t (:foreground "#cba6f7"))))
 '(line-number-current-line ((t (:foreground "#b4befe" :weight bold))))
 '(font-lock-unused-variable-face
   ((t (:foreground "#89dceb" :weight bold)))))

(with-eval-after-load 'flycheck
  (custom-set-faces
   '(flycheck-error
     ((t (:underline (:color "#cba6f7" :style line) :weight normal))))
   '(flycheck-warning
     ((t (:underline (:color "#b4befe" :style line) :weight normal))))
   '(flycheck-info
     ((t (:underline (:color "#b4befe" :style line) :weight normal))))))

(with-eval-after-load 'bookmark+
  (custom-set-faces
   '(bm-face ((t (:background "#cba6f7" :foreground "#11111b"))))
   '(bm-persistent-face ((t (:background "#cba6f7" :foreground "#11111b"))))))

(with-eval-after-load 'treemacs
  (custom-set-faces
   '(treemacs-git-added-face ((t (:foreground "#cba6f7" :weight bold))))
   '(treemacs-git-modified-face ((t (:foreground "#94e2d5" :weight bold))))
   '(treemacs-git-untracked-face ((t (:foreground "#f38ba8" :weight bold))))))

(with-eval-after-load 'lsp-ui
  (custom-set-faces
   '(lsp-ui-doc-background ((t (:background "#1e1e2e"))))
   '(lsp-ui-doc-header
     ((t (:background "#cba6f7" :foreground "#1e1e2e" :weight bold))))))

(with-eval-after-load 'web-mode
  (custom-set-faces
   '(web-mode-html-tag-face ((t (:foreground "#cba6f7"))))
   '(web-mode-html-attr-name-face ((t (:foreground "#fab387"))))
   '(web-mode-css-property-name-face ((t (:foreground "#f9e2af"))))
   '(web-mode-css-selector-face ((t (:foreground "#b4befe"))))))

(with-eval-after-load 'treemacs-nerd-icons
  (custom-set-faces
   '(treemacs-git-modified-face ((t (:foreground "#94e2d5" :weight bold))))
   '(treemacs-git-added-face ((t (:foreground "#cba6f7" :weight bold))))
   '(treemacs-git-untracked-face ((t (:foreground "#f38ba8" :weight bold))))
   '(treemacs-directory-face ((t (:foreground "#89b4fa"))))
   '(treemacs-nerd-icons-file-face ((t (:foreground "#89b4fa"))))))

(with-eval-after-load 'company
  (set-face-foreground 'company-tooltip-annotation "#cba6f7")
  (set-face-foreground 'company-tooltip-annotation-selection "#cba6f7")
  ;; (set-face-background 'company-tooltip "#11111b80")
  (set-face-foreground 'company-tooltip "#89b4fa")
  (set-face-foreground 'company-tooltip-selection "#11111b")
  (set-face-background 'company-preview "#11111b")
  (set-face-foreground 'company-preview "#cba6f7"))

(with-eval-after-load 'doom-modeline
  ;; Force all doom-modeline elements to use the same color
  ;; regardless of window focus
  (custom-set-faces
   '(doom-modeline-buffer-file ((t (:foreground "#cba6f7"))))
   '(doom-modeline-buffer-file-inactive ((t (:foreground "#cba6f7"))))

   '(doom-modeline-buffer-major-mode ((t (:foreground "#cba6f7"))))
   '(doom-modeline-buffer-major-mode-inactive ((t (:foreground "#cba6f7"))))

   '(doom-modeline-buffer-minor-mode ((t (:foreground "#cba6f7"))))
   '(doom-modeline-buffer-minor-mode-inactive ((t (:foreground "#cba6f7"))))

   '(doom-modeline-info ((t (:foreground "#cba6f7"))))
   '(doom-modeline-info-inactive ((t (:foreground "#cba6f7"))))

   '(doom-modeline-warning ((t (:foreground "#cba6f7"))))
   '(doom-modeline-warning-inactive ((t (:foreground "#cba6f7"))))

   '(doom-modeline-error ((t (:foreground "#cba6f7"))))
   '(doom-modeline-error-inactive ((t (:foreground "#cba6f7"))))

   '(doom-modeline-urgent ((t (:foreground "#cba6f7"))))
   '(doom-modeline-urgent-inactive ((t (:foreground "#cba6f7"))))

   '(doom-modeline-lsp-success ((t (:foreground "#cba6f7"))))
   '(doom-modeline-lsp-success-inactive ((t (:foreground "#cba6f7"))))

   ;; Modeline side bar
   '(doom-modeline-bar ((t (:background "#cba6f7")))))

  ;; Make active and inactive modeline identical
  (set-face-attribute 'mode-line nil
                      :background "#1e1e2e"
                      :foreground "#cba6f7")
  (set-face-attribute 'mode-line-inactive nil
                      :background "#1e1e2e"
                      :foreground "#cba6f7"))

(with-eval-after-load 'lsp-mode
  (custom-set-faces
   '(lsp-headerline-breadcrumb-path-face ((t (:foreground "#cba6f7" :weight bold))))
   '(lsp-headerline-breadcrumb-symbols-face ((t (:foreground "#89b4fa"))))))
;; change echo area text color
(with-current-buffer " *Echo Area 0*" (face-remap-add-relative 'default '(:foreground "#89b4fa")))

(provide 'faces)
;;; faces.el ends here
