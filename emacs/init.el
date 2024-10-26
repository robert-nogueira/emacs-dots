;; init.el --- Emacs Initialization File
;;; Commentary:
;; This file loads all necessary configurations for Emacs.

;;; Code:

(defvar config-dir (expand-file-name "config" user-emacs-directory))

(add-to-list 'load-path config-dir)

;; Load core modules
(load (expand-file-name "core/packages" config-dir))
(load (expand-file-name "core/keybindings" config-dir))
(load (expand-file-name "core/settings" config-dir))

;; Load completion modules
(load (expand-file-name "completion/company" config-dir))
(load (expand-file-name "completion/vertico" config-dir))
(load (expand-file-name "completion/marginalia" config-dir))
(load (expand-file-name "completion/orderless" config-dir))

;; Load programming modules
(load (expand-file-name "programming/python" config-dir))
(load (expand-file-name "programming/web-dev" config-dir))

;; Load UI modules
(load (expand-file-name "ui/interface" config-dir))
(load (expand-file-name "ui/dashboard" config-dir))
(load (expand-file-name "ui/neotree" config-dir))
(load (expand-file-name "ui/theme" config-dir))


;; ;; Load tool modules
(load (expand-file-name "tools/discord" config-dir))
(load (expand-file-name "tools/flycheck" config-dir))
(load (expand-file-name "tools/functions" config-dir))
(load (expand-file-name "tools/vc" config-dir))

;; Load miscellaneous configurations
(load (expand-file-name "misc" config-dir))
(load (expand-file-name "aliases" config-dir))

;;; init.el ends here
