;;; lua.el --- Lua / ComputerCraft configuration

(use-package lua-mode
  :mode "\\.lua\\'"
  :hook (lua-mode . lsp-deferred)
  :config
  (setq lua-indent-level 2))

(use-package apheleia
  :hook (lua-mode . apheleia-mode))

(with-eval-after-load 'lsp-mode
  ;; Disable lsp-mode's built-in Lua clients.
  (add-to-list 'lsp-disabled-clients 'lua-language-server)
  (add-to-list 'lsp-disabled-clients 'lua-roblox-language-server)

  ;; Use the Arch Linux wrapper directly.
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection "/usr/bin/lua-language-server")
    :activation-fn (lsp-activate-on "lua")
    :priority 10
    :server-id 'computercraft-lua-language-server)))

(with-eval-after-load 'lsp-lua
  ;; CC:Tweaked API stubs.
  (setq lsp-lua-workspace-library
        `((,(intern
             (expand-file-name
              "~/.local/share/lua/CC-Tweaked/library"))
           . t))))

(provide 'lua)
;;; lua.el ends here
