;;; faces.el --- Custom face settings for a personalized Emacs UI
;;; Commentary:

;; Define visual styles to match the desired theme and improve readability.

;;; Code:

(set-face-foreground 'vertical-border "#8D91E2")
(set-face-attribute 'line-number nil :height 0.7)
(set-face-attribute 'line-number-current-line nil :height 0.7)

(custom-set-faces
 '(default ((t (:background "#121324" :foreground "#D5D6F6" :height 120))))
 '(cursor ((t (:background "#C3C0D4"))))
 '(fill-column-indicator ((t (:foreground "#8D91E2" :style dotted))))
 '(line-number ((t (:foreground "#8D91E2"))))
 '(line-number-current-line ((t (:foreground "#BEBFEF" :weight bold))))
 '(font-lock-unused-variable-face
   ((t (:foreground "#9A9BE6" :weight bold)))))

(with-eval-after-load 'flycheck
  (custom-set-faces
   '(flycheck-error
     ((t (:underline (:color "#8D91E2" :style line) :weight normal))))
   '(flycheck-warning
     ((t (:underline (:color "#BEBFEF" :style line) :weight normal))))
   '(flycheck-info
     ((t (:underline (:color "#BEBFEF" :style line) :weight normal))))))

(with-eval-after-load 'bookmark+
  (custom-set-faces
   '(bm-face ((t (:background "#8D91E2" :foreground "#121324"))))
   '(bm-persistent-face ((t (:background "#8D91E2" :foreground "#121324"))))))

(with-eval-after-load 'treemacs
  (custom-set-faces
   '(treemacs-git-added-face ((t (:foreground "#8D91E2" :weight bold))))
   '(treemacs-git-modified-face ((t (:foreground "#9A9EE6" :weight bold))))
   '(treemacs-git-untracked-face ((t (:foreground "#D4CCFF" :weight bold))))))

(with-eval-after-load 'lsp-ui
  (custom-set-faces
   '(lsp-ui-doc-background ((t (:background "#292B52"))))
   '(lsp-ui-doc-header
     ((t (:background "#8D91E2" :foreground "#292B52" :weight bold))))))

(with-eval-after-load 'treemacs-nerd-icons
  (custom-set-faces
   '(treemacs-git-modified-face ((t (:foreground "#9A9EE6" :weight bold))))
   '(treemacs-git-added-face ((t (:foreground "#8D91E2" :weight bold))))
   '(treemacs-git-untracked-face ((t (:foreground "#D4CCFF" :weight bold))))
   '(treemacs-directory-face ((t (:foreground "#8D91E2"))))
   '(treemacs-nerd-icons-file-face ((t (:foreground "#8D91E2"))))))

(with-eval-after-load 'company
  (set-face-foreground 'company-tooltip-annotation "#8D91E2")
  (set-face-foreground 'company-tooltip-annotation-selection "#8D91E2")
  ;; (set-face-background 'company-tooltip "#12132480")
  (set-face-foreground 'company-tooltip "#8D91E2")
  (set-face-foreground 'company-tooltip-selection "#121324")
  (set-face-background 'company-preview "#121324")
  (set-face-foreground 'company-preview "#8D91E2"))

(with-eval-after-load 'doom-modeline
  ;; Force all doom-modeline elements to use the same color
  ;; regardless of window focus
  (custom-set-faces
   '(doom-modeline-buffer-file ((t (:foreground "#8D91E2"))))
   '(doom-modeline-buffer-file-inactive ((t (:foreground "#8D91E2"))))

   '(doom-modeline-buffer-major-mode ((t (:foreground "#8D91E2"))))
   '(doom-modeline-buffer-major-mode-inactive ((t (:foreground "#8D91E2"))))

   '(doom-modeline-buffer-minor-mode ((t (:foreground "#8D91E2"))))
   '(doom-modeline-buffer-minor-mode-inactive ((t (:foreground "#8D91E2"))))

   '(doom-modeline-info ((t (:foreground "#8D91E2"))))
   '(doom-modeline-info-inactive ((t (:foreground "#8D91E2"))))

   '(doom-modeline-warning ((t (:foreground "#8D91E2"))))
   '(doom-modeline-warning-inactive ((t (:foreground "#8D91E2"))))

   '(doom-modeline-error ((t (:foreground "#8D91E2"))))
   '(doom-modeline-error-inactive ((t (:foreground "#8D91E2"))))

   '(doom-modeline-urgent ((t (:foreground "#8D91E2"))))
   '(doom-modeline-urgent-inactive ((t (:foreground "#8D91E2"))))

   '(doom-modeline-lsp-success ((t (:foreground "#8D91E2"))))
   '(doom-modeline-lsp-success-inactive ((t (:foreground "#8D91E2"))))

   ;; Modeline side bar
   '(doom-modeline-bar ((t (:background "#8D91E2")))))

  ;; Make active and inactive modeline identical
  (set-face-attribute 'mode-line nil
                      :background "#292B52"
                      :foreground "#8D91E2")
  (set-face-attribute 'mode-line-inactive nil
                      :background "#292B52"
                      :foreground "#8D91E2"))

(with-eval-after-load 'lsp-mode
  (custom-set-faces
   '(lsp-headerline-breadcrumb-path-face ((t (:foreground "#8D91E2" :weight bold))))
   '(lsp-headerline-breadcrumb-symbols-face ((t (:foreground "#8D91E2"))))))

(provide 'faces)
;;; faces.el ends here
