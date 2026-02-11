;; fonts.el -- Configure the fonts to use.
;;
;; To install in the ui-config.el, add (require 'fonts "./ui/fonts")

;; Set the font
;; NEW

(defvar user/latin-font "Triplicate A Code"
  "Default font for Latin characters.")

(defvar user/unicode-font "Symbols Nerd Font Mono"
  "Default font for Unicode characters, including emojis.")

(defvar user/font-size 250
  "Default font size in 1/10pt (250 = 25pt).")

(defun user/set-font ()
  "Set up fonts with Unicode fallback."
  ;; Set the default font.
  (set-face-attribute 'default nil
                      :family user/latin-font
                      :height user/font-size)
  ;; Fallback to Nerd Font for symbols not in the primary font.
  ;; Nerd Font icons live in Private Use Area ranges.
  (set-fontset-font t '(#xe000 . #xffff) (font-spec :family user/unicode-font))   ; BMP PUA
  (set-fontset-font t '(#xf0000 . #xfffff) (font-spec :family user/unicode-font)) ; Supplementary PUA-A
  (set-fontset-font t 'unicode (font-spec :family user/unicode-font) nil 'append))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'user/set-font)
  (user/set-font))

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

(provide 'fonts)
