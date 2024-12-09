;; ui-config.el --- Configure the ui-components
(require 'theme "./ui/theme.el")

(use-package simple
  :straight f
  :diminish visual-line-mode
  ;; Nice Line Wrapping
  :init (setq visual-line-mode 80)
  :config (global-visual-line-mode))

;; (use-package auto-dim-other-buffers
;;   :ensure t
;;   :custom
;;   (auto-dim-other-buffers-dim-on-focus-out nil)
;;   :custom-face
;;   (auto-dim-other-buffers-face ((t (:background "#ddd"))))
;;   :config
;;   (auto-dim-other-buffers-mode nil))

;; Make a small center margin
;; (use-package fringe
;;   :straight f
;;   :init (fringe-mode 8))

;; Set the font
(defvar ui-config-fonts
  '(("Ellograph CF"          . 150)
    ("EllographCF Nerd Font" . 145)
    ("MonoLisa"              . 161)
    ("Berkeley Mono"         . 191)
    ("iA Writer Mono S"      . 151)
    ))

(defun ui-config-set-font (font)
  "Set the font for an alist value"
  (let ((font-name (car font))
        (font-size (cdr font)))
    (set-face-attribute 'default nil :height font-size :font font-name)))

(ui-config-set-font (assoc "EllographCF Nerd Font" ui-config-fonts))

(use-package ligature
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####"
                                       "~~>" "***" "||=" "||>"
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
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package adaptive-wrap
  :custom
  (adaptive-wrap-extra-indent 2)
  :init
  (adaptive-wrap-prefix-mode))

(require 'moody-config "./ui/moody-config.el")
(provide 'ui-config)
