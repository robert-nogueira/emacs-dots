;;; rust.el --- Rust Development Setup

(when (and (treesit-available-p)
           (not (treesit-language-available-p 'rust)))
  (let ((treesit-language-source-alist
         '((rust "https://github.com/tree-sitter/tree-sitter-rust"))))
    (treesit-install-language-grammar 'rust)))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook ((rust-mode . lsp-deferred)
         (rust-mode . my/rust-mode-setup)))

(defun my/rust-mode-setup ()
  "Setup rust-mode: company, tree-sitter parser, and doc comment folding."
  (setq-local company-backends '(company-capf company-dabbrev))
  (when (treesit-available-p)
    (treesit-parser-create 'rust)
    (treesit-fold-mode 1)
    (treesit-fold-line-comment-mode 1)
    (run-at-time
     0 nil
     #'my/treesit-fold-close-doc-comments
     (current-buffer))))

(defun my/treesit-fold-close-doc-comments (buf)
  "Fold all doc comment blocks in BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and (bound-and-true-p treesit-fold-mode)
                 (treesit-parser-list))
        (let ((nodes
               (treesit-query-capture
                (treesit-buffer-root-node)
                '((line_comment) @comment))))
          (dolist (capture nodes)
            (let* ((node (cdr capture))
                   (prev (treesit-node-prev-sibling node)))
              (unless (and prev
                           (string= (treesit-node-type prev) "line_comment"))
                (save-excursion
                  (goto-char (treesit-node-start node))
                  (ignore-errors
                    (treesit-fold-close node)))))))))))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package treesit-fold
  :straight (:host github :repo "emacs-tree-sitter/treesit-fold")
  :bind (:map rust-mode-map
              ("C-c f" . treesit-fold-toggle)
              ("C-c F" . treesit-fold-close-all)
              ("C-c M-f" . treesit-fold-open-all)))

(provide 'rust)
;;; rust.el ends here
