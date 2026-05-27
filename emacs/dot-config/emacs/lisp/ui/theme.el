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
  :init
  ;; (load-theme 'catppuccin :no-confirm)
  ;; (setq catppuccin-flavor 'frappe) ;; or 'frappe, 'latte, 'macchiato, or 'mocha
  ;; (catppuccin-reload)
  )

(use-package rose-pine
  :ensure t
  :straight (rose-pine
             :host github
             :repo "LuciusChen/rose-pine"
             :branch "main")
  ;; :config (load-theme 'rose-pine-night :no-confirm)
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
  
  ;; (load-theme 'sketch-black t)
  ;; (load-theme 'sketch-white t)
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
  ;; :Config (load-theme 'gruvbox-light-soft)
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

(use-package doric-themes
  :ensure t
  :demand t
  ;;   :config
  ;;   ;; These are the default values.
  ;;   (setq doric-themes-to-toggle '(doric-earth doric-fire))
  ;;   (setq doric-themes-to-rotate doric-themes-collection)

  ;;   (doric-themes-select 'doric-earth)
  )

(use-package warm-burnout
  :ensure t
  :straight (:host github :repo "felipefdl/warm-burnout" :files ("emacs/*.el"))
  :config
  ;; Preload both variants without enabling them so auto-dark can switch without
  ;; prompting about theme safety on startup.
  (load-theme 'warm-burnout-dark t t)
  (load-theme 'warm-burnout-light t t))

(use-package auto-dark
  :ensure t
  :straight (:host github :repo "LionyxML/auto-dark-emacs" :files ("auto-dark.el"))
  :after warm-burnout
  :custom
  ;; Allow macOS fallback detection when the Emacs build lacks built-in
  ;; AppleScript support.
  (auto-dark-allow-osascript t)
  ;; `auto-dark-themes' is ordered as (DARK-THEMES LIGHT-THEMES).
  (auto-dark-themes '((warm-burnout-dark) (warm-burnout-light)))
  :config
  (auto-dark-mode 1))

(provide 'theme)
