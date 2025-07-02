;;; ligatures.el --- Configuration for Ligatures
;;; Commentary:
;; This file configures ligatures in Emacs using the `ligature` package.
;; It enables various ligatures for different modes, including programming
;; and web browsing.

;;; Code:
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable ligatures in eww-mode if the `variable-pitch` face supports them
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode 
    '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
      ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
      "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
      "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
      "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
      "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
      "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
      "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
      ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
      "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
      "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
      "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
      "\\\\" "://"))
  ;; Enable ligatures globally in all buffers.
  ;; You can also enable them per mode using `ligature-mode`.
  (ligature-mode t))

(add-hook 'python-mode-hook
          (lambda ()
            (ligature-set-ligatures 'python-mode '("->" "=>" "==" "!=" ">=" "<="))
            (ligature-mode 1)))


(provide 'ligatures)
;;; ligatures.el ends here
