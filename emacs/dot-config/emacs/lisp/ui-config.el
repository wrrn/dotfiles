;; ui-config.el --- Configure the ui-components
(require 'theme "./ui/theme.el")
(require 'fonts "./ui/fonts.el")
(require 'line-wrap "./ui/line-wrap.el")
(require 'moody-config "./ui/moody-config.el")

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




(provide 'ui-config)
