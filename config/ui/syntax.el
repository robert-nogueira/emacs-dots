;;; syntax.el --- Monochromatic syntax highlighting
;;; Commentary:

;; doom-moonlight's own background and UI palette stay untouched;
;; this only flattens font-lock categories into the same blue-violet
;; monochromatic family used by the Kobalta theme (formerly
;; Monochromatic Hyprland), instead of doom-moonlight's default
;; rainbow per-category colors.

;;; Code:

(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "#575A8F" :slant italic))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#575A8F" :slant italic))))
 '(font-lock-doc-face ((t (:foreground "#575A8F" :slant italic))))

 '(font-lock-keyword-face ((t (:foreground "#8D91E2"))))
 '(font-lock-builtin-face ((t (:foreground "#8D91E2"))))
 '(font-lock-preprocessor-face ((t (:foreground "#8D91E2"))))
 '(font-lock-type-face ((t (:foreground "#8D91E2"))))

 '(font-lock-string-face ((t (:foreground "#9A9EE6"))))
 '(font-lock-constant-face ((t (:foreground "#AAAFF0"))))
 '(font-lock-function-name-face ((t (:foreground "#D4CCFF"))))
 '(font-lock-variable-name-face ((t (:foreground "#D5D6F6")))))

;; lsp-mode semantic highlighting faces doom-moonlight styles
;; explicitly, which don't inherit from font-lock-*-face above
(with-eval-after-load 'lsp-mode
  (custom-set-faces
   '(lsp-face-semhl-type-primative ((t (:foreground "#8D91E2"))))
   '(lsp-face-semhl-method ((t (:foreground "#D4CCFF"))))))

;; web-mode tag/attribute/css highlighting (moved here from
;; ui/faces.el, which had these in doom-moonlight's rainbow colors)
(with-eval-after-load 'web-mode
  (custom-set-faces
   '(web-mode-html-tag-face ((t (:foreground "#8D91E2"))))
   '(web-mode-html-attr-name-face ((t (:foreground "#D5D6F6"))))
   '(web-mode-css-property-name-face ((t (:foreground "#D5D6F6"))))
   '(web-mode-css-selector-face ((t (:foreground "#8D91E2"))))))

;; vterm's 16 ANSI colors, same values as the Kobalta terminal
;; palette (kitty), all 16 distinct, no shared hex between slots
(with-eval-after-load 'vterm
  (custom-set-faces
   '(vterm-color-black ((t (:foreground "#292B52" :background "#292B52"))))
   '(vterm-color-bright-black ((t (:foreground "#575A8F" :background "#575A8F"))))
   '(vterm-color-red ((t (:foreground "#D4CCFF" :background "#D4CCFF"))))
   '(vterm-color-bright-red ((t (:foreground "#D1CBF6" :background "#D1CBF6"))))
   '(vterm-color-green ((t (:foreground "#9A9EE6" :background "#9A9EE6"))))
   '(vterm-color-bright-green ((t (:foreground "#AAAFF0" :background "#AAAFF0"))))
   '(vterm-color-yellow ((t (:foreground "#CCCDFF" :background "#CCCDFF"))))
   '(vterm-color-bright-yellow ((t (:foreground "#AAABF0" :background "#AAABF0"))))
   '(vterm-color-blue ((t (:foreground "#8D91E2" :background "#8D91E2"))))
   '(vterm-color-bright-blue ((t (:foreground "#B6B8ED" :background "#B6B8ED"))))
   '(vterm-color-magenta ((t (:foreground "#7C81DE" :background "#7C81DE"))))
   '(vterm-color-bright-magenta ((t (:foreground "#D2D4F4" :background "#D2D4F4"))))
   '(vterm-color-cyan ((t (:foreground "#9A9BE6" :background "#9A9BE6"))))
   '(vterm-color-bright-cyan ((t (:foreground "#BEBFEF" :background "#BEBFEF"))))
   '(vterm-color-white ((t (:foreground "#BAADFF" :background "#BAADFF"))))
   '(vterm-color-bright-white ((t (:foreground "#B5AAF0" :background "#B5AAF0"))))))

(provide 'syntax)
;;; syntax.el ends here
