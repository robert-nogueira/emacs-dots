;; init.el --- Emacs Initialization File
;;; Commentary:
;; This file loads all necessary configurations for Emacs.

;;; Code:

(use-package async
  :ensure t
  :config (dired-async-mode 1))


(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 800000)))


(defvar config-dir (expand-file-name "config" user-emacs-directory))

(add-to-list 'load-path config-dir)

;; Load core modules
(load (expand-file-name "ui/interface" config-dir))
(load (expand-file-name "core/packages" config-dir))
(load (expand-file-name "core/keybindings" config-dir))
(load (expand-file-name "core/settings" config-dir))

;; Load UI modules
(load (expand-file-name "ui/dashboard" config-dir))
(load (expand-file-name "ui/neotree" config-dir))
(load (expand-file-name "ui/theme" config-dir))

;; Load programming modules
(load (expand-file-name "programming/python" config-dir))
(load (expand-file-name "programming/web-dev" config-dir))

;; Load completion modules
(load (expand-file-name "completion/company" config-dir))
(load (expand-file-name "completion/vertico" config-dir))
(load (expand-file-name "completion/marginalia" config-dir))
(load (expand-file-name "completion/orderless" config-dir))

;; ;; Load tool modules
(load (expand-file-name "tools/flycheck" config-dir))
(load (expand-file-name "tools/functions" config-dir))
(load (expand-file-name "tools/vc" config-dir))
(load (expand-file-name "tools/discord" config-dir))

;; Load miscellaneous configurations
(load (expand-file-name "misc" config-dir))
(load (expand-file-name "aliases" config-dir))

;; (use-package telephone-line
;;   :ensure t)

;; (telephone-line-mode t)

;; ;; Cores da modeline usando o tema Catppuccin Mocha com roxo
;; (set-face-background 'mode-line "#1E1E2E")         ; Cor de fundo ativa
;; (set-face-foreground 'mode-line "#F5E0DC")         ; Cor do texto ativo
;; (set-face-background 'mode-line-inactive "#26233A") ; Cor de fundo inativa
;; (set-face-foreground 'mode-line-inactive "#C6C6D5") ; Cor do texto inativo
;; (set-face-foreground 'mode-line '#DDB6F2)           ; Cor do texto ativo com tom de roxo

(display-time-mode 1)
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "a5b8812270156398a2d93358c0ffd9525fc4fcc4ecb9844aa040e54613146a24" "1114c56feb07fb44ad12ede602c566a1387aeffcf5990a446952d54fba620be3" "24b6ade0e3cabdfee9fa487961b089d059e048d77fe13137ea4788c1b62bd99d" "4d16802de4686030ed8f30b5a844713d68edec9cc07322bef54493d15e68d8cd" "11feb87b02688866cef2199e268cad5f6d473ebacaa5f06c35c3ac08894a2845" "3ec12a9bce6b2ff1d805593de06e012b1d999963ed8e75750760eb6bab7b0092" "7bcbee7c5eaef0eca0dad6c5ec0ee8f665efd857be421a67be1ce84003d71417" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; (set-frame-parameter nil 'alpha-background 90)
;; (add-to-list 'default-frame-alist '(alpha-background . 90))

