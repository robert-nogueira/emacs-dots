;;; formatting.el --- Code formatting configuration

(use-package apheleia
  :demand t
  :config
  ;; Python
  (setf (alist-get 'python-mode apheleia-mode-alist)
        'ruff)

  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        'ruff)

  ;; Rust
  (setf (alist-get 'rustfmt-2024 apheleia-formatters)
        '("rustfmt"
          "--quiet"
          "--edition" "2024"
          "--emit" "stdout"))

  (setf (alist-get 'rust-mode apheleia-mode-alist)
        'rustfmt-2024)

  (setf (alist-get 'rust-ts-mode apheleia-mode-alist)
        'rustfmt-2024)

  ;; Lua
  (setf (alist-get 'stylua-project apheleia-formatters)
        '("apheleia-from-project-root"
          "stylua.toml"
          "stylua"
          "--stdin-filepath"
          filepath
          "-"))

  (setf (alist-get 'lua-mode apheleia-mode-alist)
        'stylua-project)

  (setf (alist-get 'lua-ts-mode apheleia-mode-alist)
        'stylua-project)

  ;; JavaScript
  (setf (alist-get 'js-mode apheleia-mode-alist)
        'prettier-javascript)

  (setf (alist-get 'js-ts-mode apheleia-mode-alist)
        'prettier-javascript)

  ;; TypeScript
  (setf (alist-get 'typescript-mode apheleia-mode-alist)
        'prettier-typescript)

  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist)
        'prettier-typescript)

  ;; TSX
  (setf (alist-get 'tsx-mode apheleia-mode-alist)
        'prettier-typescript)

  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist)
        'prettier-typescript)

  ;; Web
  (setf (alist-get 'web-mode apheleia-mode-alist)
        'prettier)

  (setf (alist-get 'css-mode apheleia-mode-alist)
        'prettier-css)

  (setf (alist-get 'css-ts-mode apheleia-mode-alist)
        'prettier-css)

  ;; Enable format-on-save globally for modes configured above.
  (apheleia-global-mode +1))

(provide 'formatting)
;;; formatting.el ends here
