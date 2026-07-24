;;; syntax.el --- Monochromatic syntax highlighting
;;; Commentary:

;; doom-moonlight's own background and UI palette stay untouched;
;; this only flattens font-lock categories into the same blue-violet
;; monochromatic family used by the Monochromatic Hyprland VS Code
;; theme, instead of doom-moonlight's default rainbow per-category
;; colors.

;;; Code:

(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "#575A8F" :slant italic))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#575A8F" :slant italic))))
 '(font-lock-doc-face ((t (:foreground "#575A8F" :slant italic))))

 '(font-lock-keyword-face ((t (:foreground "#9A9EE6"))))
 '(font-lock-builtin-face ((t (:foreground "#9A9EE6"))))
 '(font-lock-preprocessor-face ((t (:foreground "#9A9EE6"))))
 '(font-lock-type-face ((t (:foreground "#9A9EE6"))))

 '(font-lock-string-face ((t (:foreground "#CCCFFF"))))
 '(font-lock-constant-face ((t (:foreground "#AAAFF0"))))
 '(font-lock-function-name-face ((t (:foreground "#D4CCFF"))))
 '(font-lock-variable-name-face ((t (:foreground "#FFFFFF")))))

;; lsp-mode semantic highlighting faces doom-moonlight styles
;; explicitly, which don't inherit from font-lock-*-face above
(with-eval-after-load 'lsp-mode
  (custom-set-faces
   '(lsp-face-semhl-type-primative ((t (:foreground "#9A9EE6"))))
   '(lsp-face-semhl-method ((t (:foreground "#D4CCFF"))))))

;; web-mode tag/attribute/css highlighting (moved here from
;; ui/faces.el, which had these in doom-moonlight's rainbow colors)
(with-eval-after-load 'web-mode
  (custom-set-faces
   '(web-mode-html-tag-face ((t (:foreground "#9A9EE6"))))
   '(web-mode-html-attr-name-face ((t (:foreground "#FFFFFF"))))
   '(web-mode-css-property-name-face ((t (:foreground "#FFFFFF"))))
   '(web-mode-css-selector-face ((t (:foreground "#9A9EE6"))))))

(provide 'syntax)
;;; syntax.el ends here
