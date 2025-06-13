;;; discord.el --- Elcord Configuration for Discord Rich Presence

;;; Code:

(use-package elcord
  :straight (elcord :type git :host github :repo "robert-nogueira/elcord-weeb")
  :init
  (setq elcord-icon-base "https://raw.githubusercontent.com/robert-nogueira/elcord-weeb/master/icons/")
  (setq elcord-client-id "1382406746445709412")

  ;; Custom mode icons
  (setq elcord-mode-icon-alist
        '((python-mode . "bocchi_python-mode_icon")
          (typescript-mode . "ryo_typescript-mode_icon")
          (rust-mode . "ikuyo_rust-mode_icon")
          ;; Keep all other default mappings
          (agda-mode . "agda-mode_icon")
          (assembly-mode . "assembly-mode_icon")
          (bqn-mode . "bqn-mode_icon")
          (c-mode . "c-mode_icon")
          (c++-mode . "cpp-mode_icon")
          (clojure-mode . "clojure-mode_icon")
          (csharp-mode . "csharp-mode_icon")
          (comint-mode . "comint-mode_icon")
          (cperl-mode . "cperl-mode_icon")
          (dockerfile-mode . "dockerfile-mode_icon")
          (elixir-mode . "elixir-mode_icon")
          (emacs-lisp-mode . (elcord--editor-icon))
          (enh-ruby-mode . "ruby-mode_icon")
          (erc-mode . "irc-mode_icon")
          (erlang-mode . "erlang-mode_icon")
          (forth-mode . "forth-mode_icon")
          (fortran-mode . "fortran-mode_icon")
          (fsharp-mode . "fsharp-mode_icon")
          (gdscript-mode . "gdscript-mode_icon")
          (haskell-mode . "haskell-mode_icon")
          (haskell-interactive-mode . "haskell-mode_icon")
          (hy-mode . "hy-mode_icon")
          (java-mode . "java-mode_icon")
          (julia-mode . "julia-mode_icon")
          (js-mode . "javascript-mode_icon")
          (kotlin-mode . "kotlin-mode_icon")
          (go-mode . "go-mode_icon")
          (latex-mode . "latex-mode_icon")
          (lisp-mode . "lisp-mode_icon")
          (lua-mode . "lua-mode_icon")
          (magit-mode . "magit-mode_icon")
          (markdown-mode . "markdown-mode_icon")
          (meson-mode . "meson-mode_icon")
          (nasm-mode . "nasm-mode_icon")
          (nim-mode . "nim-mode_icon")
          (nix-mode . "nix-mode_icon")
          (ocaml-mode . "ocaml-mode_icon")
          (octave-mode . "octave-mode_icon")
          (org-mode . "org-mode_icon")
          (pascal-mode . "pascal-mode_icon")
          (php-mode . "php-mode_icon")
          (prolog-mode . "prolog-mode_icon")
          (puml-mode . "puml-mode_icon")
          (puppet-mode . "puppet-mode_icon")
          (racket-mode . "racket-mode_icon")
          (ruby-mode . "ruby-mode_icon")
          (rustic-mode . "rust-mode_icon")
          (scala-mode . "scala-mode_icon")
          (solidity-mode . "solidity-mode_icon")
          (sh-mode . "comint-mode_icon")
          (terraform-mode . "terraform-mode_icon")
          (zig-mode . "zig-mode_icon")
          (janet-mode . "janet-mode_icon")
          ("^slime-.*" . "lisp-mode_icon")
          ("^sly-.*$" . "lisp-mode_icon")))

  :config
  (setq elcord-switch-icons t)
  (setq elcord-refresh-rate 3)
  (setq elcord-editor-icon "marisa_kirisame_emacs")

  ;; Override the presence function to add customized buttons
  (defun elcord--set-presence ()
    "Set presence with customized buttons."
    (let* ((activity
            `(("assets" . (,@(elcord--mode-icon-and-text)))
              ,@(elcord--details-and-state)
              ("buttons" . [
			    ((:label . "📂 github")
			     (:url . "https://github.robertnogueira.com"))
			    ])))
	   (nonce (format-time-string "%s%N"))
	   (presence
	    `(("cmd" . "SET_ACTIVITY")
              ("args" . (("activity" . ,activity)
			 ("pid" . ,(emacs-pid))))
              ("nonce" . ,nonce))))
      (elcord--send-packet 1 presence)))
  (elcord-mode))

(provide 'discord)
;;; discord.el ends here
