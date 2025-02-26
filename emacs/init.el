;; init.el --- Emacs Initialization File
;;; Commentary:
;; This file loads all necessary configurations for Emacs.

;;; Code:

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
(load (expand-file-name "tools/treemacs" config-dir))
(load (expand-file-name "ui/theme" config-dir))


;; Load completion modules
(load (expand-file-name "completion/company" config-dir))
(load (expand-file-name "completion/vertico" config-dir))
(load (expand-file-name "completion/marginalia" config-dir))
(load (expand-file-name "completion/orderless" config-dir))

;; ;; Load tool modules
(load (expand-file-name "tools/centaur-tabs" config-dir))
(load (expand-file-name "tools/flycheck" config-dir))
(load (expand-file-name "tools/functions" config-dir))
(load (expand-file-name "tools/discord" config-dir))
(load (expand-file-name "tools/vc" config-dir))
(load (expand-file-name "tools/projectile" config-dir))
(load (expand-file-name "tools/vterm" config-dir))

;; Load programming modules
(load (expand-file-name "programming/python" config-dir))
(load (expand-file-name "programming/web-dev" config-dir))

;; Load miscellaneous configurations
(load (expand-file-name "misc" config-dir))
(load (expand-file-name "aliases" config-dir))

;;; init.el ends here
;; (set-frame-parameter nil 'alpha-background 90)
;; (add-to-list 'default-frame-alist '(alpha-background . 90))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-safe-themes
   '("189b44ac85bbcfbbf8886eb14925c10a6f09f6485b7e3c19503aa44131de2999" "b9c002dc827fb75b825da3311935c9f505d48d7ee48f470f0aa7ac5d2a595ab2" "90185f1d8362727f2aeac7a3d67d3aec789f55c10bb47dada4eefb2e14aa5d01" "b1791a921c4f38cb966c6f78633364ad880ad9cf36eef01c60982c54ec9dd088" "a3a71b922fb6cbf9283884ac8a9109935e04550bcc5d2a05414a58c52a8ffc47" "fae5872ff90462502b3bedfe689c02d2fa281bc63d33cb007b94a199af6ccf24" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "ae20535e46a88faea5d65775ca5510c7385cbf334dfa7dde93c0cd22ed663ba0" "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "25065e711dcb51ad5bc3d691cedd7162c566b38e777604785d0f09a0d7cde6c4" "6454421996f0508c38215a633256e36c19a28591542fb0946cfc40f1dceb89cf" "e9aa348abd3713a75f2c5ba279aa581b1c6ec187ebefbfa33373083ff8004c7c" "4d16802de4686030ed8f30b5a844713d68edec9cc07322bef54493d15e68d8cd" "a7026ae6351ed42b2e71f373e173e6d9da5bd0c5461dc1861aa74c1a247c1a97" "11819dd7a24f40a766c0b632d11f60aaf520cf96bd6d8f35bae3399880937970" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478" "71ef2c8ced402aa3cd319a799e748631a1c080a8aeb0852469c589b59547be76" "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "d481904809c509641a1a1f1b1eb80b94c58c210145effc2631c1a7f2e4a2fdf4" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "7bcbee7c5eaef0eca0dad6c5ec0ee8f665efd857be421a67be1ce84003d71417" "11feb87b02688866cef2199e268cad5f6d473ebacaa5f06c35c3ac08894a2845" default)))
