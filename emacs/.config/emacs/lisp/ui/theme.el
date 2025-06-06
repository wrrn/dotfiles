;; theme.el -- Configure the them we want to use

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
  ;; :init (load-theme 'rose-pine-moon t)
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
  :ensure t
  ;; :config (load-theme 'goose)
  )

(use-package gruvbox-theme
  :ensure t
  ;; :config (load-theme 'gruvbox-light-soft)
  )

;; (use-package kanagawa-theme
;;   :ensure t
;;   :straight (kanagawa-theme :type git :host github :repo  "jasonm23/emacs-theme-kanagawa")
;;   :requires autothemer
;;   :config (load-theme 'kanagawa)
;;   :custom-face
;;   (fringe ((t (:background "#181820")))))

(use-package an-old-hope-theme
  :ensure t
  :straight (an-old-hope-theme
             :type git
             :host github
             :repo "mohkale/an-old-hope-emacs"
             :name an-old-hope-theme)
  ;; :config (load-theme 'an-old-hope))
  )
(use-package modus-themes
  :ensure t
  ;; :config (load-theme 'modus-vivendi-tinted)
  )
(provide 'theme)
