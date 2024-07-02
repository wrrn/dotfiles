;; ui-config.el --- Configure the ui-components
(use-package autothemer
  :ensure t)

(use-package minimal-theme
  :ensure t
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



(defun nano-modeline-vterm-shell-mode ()
  "Show COPY if vterm is in copy mode"
  (propertize (if vterm-copy-mode
                  "(copy mode)"
                "")
              'face (nano-modeline-face 'primary)))

(defun nano-modeline-vterm-mode ()
  "Nano line for vterm-mode"
  (funcall nano-modeline-position
           '((nano-modeline-buffer-status ">_") " "
             (nano-modeline-term-shell-name) " "
             (nano-modeline-vterm-shell-mode))
           '((nano-modeline-default-directory) " "
             (nano-modeline-window-dedicated))))

(use-package nano-modeline
  :ensure t
  :config (setq-default mode-line-format nil)
  :hook
  (prog-mode            . nano-modeline-prog-mode)
  (text-mode            . nano-modeline-text-mode)
  (org-mode             . nano-modeline-org-mode)
  (pdf-view-mode        . nano-modeline-pdf-mode)
  (mu4e-headers-mode    . nano-modeline-mu4e-headers-mode)
  (mu4e-view-mode       . nano-modeline-mu4e-message-mode)
  (mu4e-compose-mode    . nano-modeline-mu4e-compose-mode)
  (elfeed-show-mode     . nano-modeline-elfeed-entry-mode)
  (elfeed-search-mode   . nano-modeline-elfeed-search-mode)
  (elpher-mode          . nano-modeline-elpher-mode)
  (term-mode            . nano-modeline-term-mode)
  (vterm-mode           . nano-modeline-vterm-mode)
  (eat-mode             . nano-modeline-eat-mode)
  (xwidget-webkit-mode  . nano-modeline-xwidget-mode)
  (messages-buffer-mode . nano-modeline-message-mode)
  (org-capture-mode     . nano-modeline-org-capture-mode)
  (org-agenda-mode      . nano-modeline-org-agenda-mode))

(use-package simple
  :straight f
  :diminish visual-line-mode
  ;; Nice Line Wrapping
  :init (setq visual-line-mode 80)
  :config (global-visual-line-mode))

;; Make a small center margin
(use-package fringe
  :straight f
  :init (fringe-mode 8))

;; Set the font
(defvar ui-config-fonts
  '(("MonoLisa"         . 161)
    ("Berkeley Mono"    . 191)
    ("iA Writer Mono S" . 151)
    ))

(defun ui-config-set-font (font)
  "Set the font for an alist value"
  (let ((font-name (car font))
        (font-size (cdr font)))
    (set-face-attribute 'default nil :height font-size :font font-name)))

(ui-config-set-font (assoc "MonoLisa" ui-config-fonts))

(use-package ligature
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
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


(require 'nano-modeline)
(set-face-attribute 'vertical-border nil
                    :foreground (face-background 'nano-modeline-active)
                    :strike-through (face-background 'nano-modeline-active)
                    :underline (face-background 'nano-modeline-active)
                    :overline (face-background 'nano-modeline-active))
;; ;; (use-package mini-frame
;;   :ensure t
;;   :config (mini-frame-mode)
;;   ;; :custom
;; (mini-frame-show-parameters '((top . 10)
;;                               (width . 0.7)
;;                               (left . 0.5)))
;; )

;; (use-package nano-minibuffer
;;   :ensure t
;;   :straight (nano-minibuffer :type git :host github :repo "rougier/nano-minibuffer"
;;                              :build (:not compile))
;;   :custom nano-minibuffer-position 'bottom)

;;(use-package nano
;;:ensure t
;;:straight (nano :type git :host github :repo "rougier/nano-emacs"
;; :fork (:host github
;; :repo "wrrn/nano-emacs")
;;              :build (:not compile)))

;; :config (progn
;;           (set-frame-parameter nil 'internal-border-width 15)
;;           (custom-set-faces
;;            `(window-divider ((t (:foreground ,(face-attribute 'default :background)))))
;;            '(window-divider-first-pixel  ((t (:inherit window-divider))))
;;            '(window-divider-last-pixel  ((t (:inherit window-divider))))
;;            '(fringe  ((t (:inherit window-divider)))))
;;           (set-face-attribute 'mode-line nil
;;                               :foreground (face-foreground 'nano-face-subtle)
;;                               :background (face-foreground 'nano-face-subtle)
;;                               :inherit nil
;;                               :box nil)
;;           (set-face-attribute 'mode-line-inactive nil
;;                               :foreground (face-foreground 'nano-face-subtle)
;;                               :background (face-foreground 'nano-face-subtle)
;;                               :inherit nil
;;                               :box nil)))

;; Set default font. First one found is selected.
;; (defun font-existsp (font)
;;   "Check to see if the named FONT is available."
;;   (if (null (x-list-fonts font))
;;       nil t))
;; (cond
;;  ((eq window-system nil) nil)
;;  ((font-existsp "MonoLisa")
;;   (set-face-attribute 'default nil :height 181 :font "MonoLisa"))
;;  ((font-existsp "Hurmit Nerd Font Mono")
;;   (set-face-attribute 'default nil :height 181 :font "Hurmit Nerd Font Mono"))
;;  ((font-existsp "Berkeley Mono")
;;   (set-face-attribute 'default nil :height 191 :font "Berkeley Mono"))
;;  ((font-existsp "iA Writer Mono S")
;;   (set-face-attribute 'default nil :height 151 :font "iA Writer Mono S"))
;;  ((font-existsp "Comic Code")
;;   (set-face-attribute 'default nil :height 131 :font "Comic Code"))
;;  ((font-existsp "Comic Mono")
;;   (set-face-attribute 'default nil :height 151 :font "Comic Mono"))
;;  ((font-existsp "Jetbrains Mono")
;;   (set-face-attribute 'default nil :height 151 :font "Jetbrains Mono"))
;;  ((font-existsp "Roboto Mono")
;;   (set-face-attribute 'default nil :height 131 :font "Roboto Mono"))
;;  ((font-existsp "Input Mono Compressed")
;;   (set-face-attribute 'default nil :height 131 :font "Input Mono Compressed"))
;;  ((font-existsp "PragmataPro")
;;   (set-face-attribute 'default nil :height 131 :font "PragmataPro"))
;;  ((font-existsp "Source Code Pro")
;;   (set-face-attribute 'default nil :height 121 :font "Source Code Pro"))
;;  ((font-existsp "Menlo")
;;   (set-face-attribute 'default nil :height 121 :font "Menlo"))
;;  ((font-existsp "Consolas")
;;   (set-face-attribute 'default nil :height 121 :font "Consolas"))
;;  ((font-existsp "Inconsolata")
;;   (set-face-attribute 'default nil :height 121 :font "Inconsolata"))
;;  ((font-existsp "Envy Code R")
;;   (set-face-attribute 'default nil :height 121 :font "Envy Code R"))
;;  )

;; This assumes you've installed the package via MELPA.

(provide 'ui-config)
