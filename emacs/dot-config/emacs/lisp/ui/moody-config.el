;; moody-config.el --- configure the moody mode-line
;;
;; To install in the ui-config.el, add (require 'moody-config "./ui/moody-config")
(defvar-local moody-mode-line-persp-identification
    '(:eval (moody-tab (persp-current-name)
                       20 'down)))
(put 'moody-mode-line-persp-identification 'risky-local-variable t)

(use-package moody
  :ensure t
  :config
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (setq x-underline-at-descent-line t))

(require 'moody)

(setq-default mode-line-format '("%e" moody-mode-line-front-space
                                 (:propertize
                                  ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
                                  display
                                  (min-width
                                   (5.0)))
                                 

                                 mode-line-frame-identification moody-mode-line-persp-identification "  " "%b" "   "
                                 mode-line-position
                                 (vc-mode moody-vc-mode)))

(redisplay)


(provide 'moody-config)
