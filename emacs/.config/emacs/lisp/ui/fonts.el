;; fonts.el -- Configure the fonts to use.
;;
;; To install in the ui-config.el, add (require 'fonts "./ui/fonts")

;; Set the font
(defun font-exists-p (font-name)
  "Check if FONT-NAME exists in both GUI and terminal."
  (if (display-graphic-p)
      (if (x-list-fonts font-name) t nil)
    (if (font-info font-name) t nil)))


(defun ui-config-set-font (name size)
  "Set the font to the given name and size. Returns true if the font exists"
  (if (font-exists-p name)
      (progn
        (set-face-attribute 'default nil :height size :font name)
        t)))


(defvar ui-config-fonts
  '(
    (ui-config-set-font "MonoLisa"              135)
    (ui-config-set-font "Berkeley Mono"         151)
    (ui-config-set-font "EllographCF Nerd Font" 100)
    (ui-config-set-font "Ellograph CF"          100)
    (ui-config-set-font "iA Writer Mono S"      151)
    ))

(defun ui-config-set-frame-font (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (let ((fonts ui-config-fonts))
      (while fonts
        (if (eval (car fonts))
            (progn
              (setq fonts nil)
              (message "font set"))
          (setq fonts (cdr fonts)))))))


;; Trying setting the font until we find a font that is avaiable.
(add-hook 'after-make-frame-functions #'ui-config-set-frame-font)
(add-hook 'window-setup-hook #'ui-config-set-frame-font)

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
