;; ui-config.el --- Configure the ui-components
(require 'theme "./ui/theme.el")
(require 'fonts "./ui/fonts.el")

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



(use-package adaptive-wrap
  :straight (adaptive-wrap
             :type git
             :host github
             :repo "emacs-straight/adaptive-wrap"
             :fork (:host git.sr.ht :repo "~warren/adaptive-wrap"))

  :custom
  (adaptive-wrap-extra-indent 2)
  :init
  (adaptive-wrap-prefix-mode))

(require 'moody-config "./ui/moody-config.el")
(provide 'ui-config)
