;; fonts.el -- Configure the fonts to use.
;;
;; To install in the ui-config.el, add (require 'fonts "./ui/fonts")

;; Set the font
;; NEW

(defvar user/latin-font "Triplicate A Code"
  "Default font for Latin characters.")

(defvar user/unicode-font "Symbols Nerd Font Mono"
  "Default font for Unicode characters, including emojis.")

(defvar user/font-size 14
  "Default font size in px.")

(defvar user/standard-fontset
  (create-fontset-from-fontset-spec standard-fontset-spec)
  "Standard fontset for user.")

;; Ensure user/standard-fontset gets used for new frames.
(add-to-list 'default-frame-alist (cons 'font user/standard-fontset))
(add-to-list 'initial-frame-alist (cons 'font user/standard-fontset))

(defun user/set-font ()
  "Set Unicode, Latin and CJK font for user/standard-fontset."
  ;; Unicode font.
  (set-fontset-font user/standard-fontset 'unicode
                    (font-spec :family user/unicode-font)
                    nil 'prepend)
  ;; Latin font.
  ;; Only specify size here to allow text-scale-adjust work on other fonts.
  (set-fontset-font user/standard-fontset 'latin
                    (font-spec :family user/latin-font :size user/font-size)
                    nil 'prepend))

(user/set-font)
(set-frame-font user/standard-fontset)

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
