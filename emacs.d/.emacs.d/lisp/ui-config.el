;; ui-config.el --- Configure the ui-components
(use-package autothemer
  :ensure t)

;; (use-package minimal-theme
;;     :ensure t
;;     :config (load-theme 'minimal-light t))

;; (use-package almost-mono-themes
;;   :config
;;   ;; (load-theme 'almost-mono-black t)
;;   ;; (load-theme 'almost-mono-gray t)
;;   (load-theme 'almost-mono-cream t)
;;   ;; (load-theme 'almost-mono-white t)
;;   )

;; (use-package catppuccin-theme
;;   :init
;;   (load-theme 'catppuccin :no-confirm)
;;   (setq catppuccin-flavor 'mocha) ;; or 'frappe, 'latte, 'macchiato, or 'mocha
;;   (catppuccin-reload)
;;   )

;; (use-package kaolin-themes
;;   :ensure t
;;   :config
;;   (load-theme 'kaolin-valley-light :no-confirm)
;;   (kaolin-treemacs-theme))

;; (use-package sketch-themes
;;   :config
;;   ;; Load black version
;;   ;; (load-theme 'sketch-black t)
;;   ;; Load white version
;;   (load-theme 'sketch-white t))

;; (use-package subatomic-theme
;; :ensure t
;; :config (load-theme 'subatomic))

;; (use-package rose-pine-emacs
;;   :ensure t
;;   :straight (rose-pine-emacs :type git :host github :repo "thongpv87/rose-pine-emacs")
;;   :requires autothemer
;;   :config (load-theme 'rose-pine-moon))
;; (load-theme 'rose-pine-moon)

;; (use-package darktooth-theme
;;   :ensure t
;;   :config (load-theme 'darktooth))

;; (use-package soothe-theme
;;   :ensure t
;;   :config (load-theme 'soothe))

;; (use-package jazz-theme
;;   :ensure t
;;   :config (load-theme 'jazz))

;; (use-package flatland-theme
;; :ensure t)
;; (use-package goose-theme
;; :ensure t)
;; (use-package nano-theme
;;   :ensure t
;;   :config (load-theme 'nano-light))

;; (use-package kanagawa-theme
;;   :straight (kanagawa-theme :type git :host github :repo  "jasonm23/emacs-theme-kanagawa")
;;   :requires autothemer
;;   :config (load-theme 'kanagawa))

(use-package modus-themes
  :ensure t
  :config (load-theme 'modus-vivendi-tinted))

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
  (eat-mode             . nano-modeline-eat-mode)
  (xwidget-webkit-mode  . nano-modeline-xwidget-mode)
  (messages-buffer-mode . nano-modeline-message-mode)
  (org-capture-mode     . nano-modeline-org-capture-mode)
  (org-agenda-mode      . nano-modeline-org-agenda-mode))

;; (use-package mini-frame
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

(defun font-existsp (font)
  "Check to see if the named FONT is available."
  (if (null (x-list-fonts font))
      nil t))

;; Set default font. First one found is selected.
(cond
 ((eq window-system nil) nil)
 ;; ((font-existsp "iA Writer Mono S")
 ;; (set-face-attribute 'default nil :height 151 :font "iA Writer Mono S"))
 ((font-existsp "Ellograph CF")
  (set-face-attribute 'default nil :height 151 :font "Ellograph CF"))
 ((font-existsp "Comic Code")
  (set-face-attribute 'default nil :height 131 :font "Comic Code"))
 ((font-existsp "Comic Mono")
  (set-face-attribute 'default nil :height 151 :font "Comic Mono"))
 ((font-existsp "Jetbrains Mono")
  (set-face-attribute 'default nil :height 151 :font "Jetbrains Mono"))
 ((font-existsp "Roboto Mono")
  (set-face-attribute 'default nil :height 131 :font "Roboto Mono"))
 ((font-existsp "Input Mono Compressed")
  (set-face-attribute 'default nil :height 131 :font "Input Mono Compressed"))
 ((font-existsp "PragmataPro")
  (set-face-attribute 'default nil :height 131 :font "PragmataPro"))
 ((font-existsp "Source Code Pro")
  (set-face-attribute 'default nil :height 121 :font "Source Code Pro"))
 ((font-existsp "Menlo")
  (set-face-attribute 'default nil :height 121 :font "Menlo"))
 ((font-existsp "Consolas")
  (set-face-attribute 'default nil :height 121 :font "Consolas"))
 ((font-existsp "Inconsolata")
  (set-face-attribute 'default nil :height 121 :font "Inconsolata"))
 ((font-existsp "Envy Code R")
  (set-face-attribute 'default nil :height 121 :font "Envy Code R"))
 )


(provide 'ui-config)
