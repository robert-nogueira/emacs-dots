;;; hooks-lsp-ui.el --- LSP UI hook configuration

;;; Commentary:
;; Key remaps for lsp-ui and global doc show keybinding.

;;; Code:

(with-eval-after-load 'lsp-ui
  (define-key lsp-ui-mode-map
	      [remap xref-find-definitions]
	      #'lsp-ui-peek-find-definitions)

  (define-key lsp-ui-mode-map
	      [remap xref-find-references]
	      #'lsp-ui-peek-find-references)

  (global-set-key (kbd "C-c C-d") 'lsp-ui-doc-show))

(provide 'hooks-lsp-ui)
;;; hooks-lsp-ui.el ends here
