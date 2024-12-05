;; ui-config.el --- Configure the ui-components

(use-package autothemer
  :ensure t)

(use-package minimal-theme
  ;; :ensure t
  ;; :config (load-theme 'minimal-light t)
  )

(use-package almost-mono-themes
  :ensure t
  ;; :config
  ;; (load-theme 'almost-mono-black t)
  ;; (load-theme 'almost-mono-gray t)
  ;; (load-theme 'almost-mono-cream t)
  ;; (load-theme 'almost-mono-white t)
  )

(use-package catppuccin-theme
  :ensure t
  ;; :init
  ;; (load-theme 'catppuccin :no-confirm)
  ;; (setq catppuccin-flavor 'frappe) ;; or 'frappe, 'latte, 'macchiato, or 'mocha
  ;; (catppuccin-reload)
  )

(use-package rose-pine-color-theme
  :ensure t
  :straight (rose-pine-emacs
             :host github
             :repo "thongpv87/rose-pine-emacs"
             :branch "master")
  :requires autothemer
  :init (load-theme 'rose-pine-dawn t))


(use-package kaolin-themes
  :ensure t
  ;;   :config
  ;;   (load-theme 'kaolin-valley-light :no-confirm)
  ;;   (kaolin-treemacs-theme)
  )

(use-package sketch-themes
  :ensure t
  :config
  ;; Load black version
  ;; (load-theme 'sketch-black t)
  ;; Load white version
  (load-theme 'sketch-white t)
  )

(use-package subatomic-theme
  :ensure t
  ;; :config (load-theme 'subatomic)
  )

(use-package darktooth-theme
  :ensure t
  ;;   :config (load-theme 'darktooth)
  )

(use-package soothe-theme
  :ensure t
  ;; :config (load-theme 'soothe)
  )

(use-package jazz-theme
  :ensure t
  ;;   :config (load-theme 'jazz)
  )

(use-package goose-theme
  :ensure t)

(use-package gruvbox-theme
  :ensure t
  ;; :config (load-theme 'gruvbox-light-soft)
  )

(use-package kanagawa-theme
  :ensure t
  :straight (kanagawa-theme :type git :host github :repo  "jasonm23/emacs-theme-kanagawa")
  :requires autothemer
  ;; :config (load-theme 'kanagawa)
  )

(use-package modus-themes
  :ensure t
  ;; :config (load-theme 'modus-vivendi-tinted)
  )

(use-package simple
  :straight f
  :diminish visual-line-mode
  ;; Nice Line Wrapping
  :init (setq visual-line-mode 80)
  :config (global-visual-line-mode))

(use-package auto-dim-other-buffers
  :ensure t
  :custom
  (auto-dim-other-buffers-dim-on-focus-out nil)
  :custom-face
  (auto-dim-other-buffers-face ((t (:background "#ddd"))))
  :config
  (auto-dim-other-buffers-mode t))

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
