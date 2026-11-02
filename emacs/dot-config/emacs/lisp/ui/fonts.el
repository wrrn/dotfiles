;; fonts.el -- Configure the fonts to use.
;;
;; To install in the ui-config.el, add (require 'fonts "./ui/fonts")

;; Set the font
;; NEW

(defvar user/latin-font "Triplicate A Code"
  "Default font for Latin characters.")

(defvar user/unicode-font "Symbols Nerd Font Mono"
  "Default fallback font for Unicode symbols.")

(defvar user/emoji-font "OpenMoji Black"
  "Font used for emoji characters not provided by the symbol font.")

(defvar user/prose-font "Century Supra A"
  "Proportional font for prose (used by mixed-pitch).")

(defvar user/font-size 180
  "Default font size in 1/10pt (250 = 25pt).")

(defun user/set-font ()
  "Set up fonts with Unicode fallback."
  (set-face-attribute 'default nil
                      :family user/latin-font
                      :height user/font-size)
  ;; Code, tables etc. under mixed-pitch should match the default font.
  (set-face-attribute 'fixed-pitch nil :family user/latin-font :height 1.0)
  ;; Prose font; height is relative to default so it follows user/font-size.
  (set-face-attribute 'variable-pitch nil :family user/prose-font :height 1.05)
  (set-fontset-font t '(#xe000 . #xffff) (font-spec :family user/unicode-font))
  (set-fontset-font t '(#xf0000 . #xfffff) (font-spec :family user/unicode-font))
  (set-fontset-font t #x1f441 (font-spec :family user/emoji-font) nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family user/unicode-font) nil 'append))

(use-package mixed-pitch
  :ensure t
  :custom (mixed-pitch-set-height t)
  :hook (markdown-modern-mode . mixed-pitch-mode))

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
