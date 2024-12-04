;; nano-config.el --- Configure the nano-modeline
;;
;; To install in the ui-config.el, add (require 'nano-config "./ui/nano-config")

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
             (nano-modeline-buffer-name) " "
             (nano-modeline-vterm-shell-mode))
           '((nano-modeline-default-directory) " "
             (nano-modeline-window-dedicated))))

(use-package nano-modeline
  :ensure t
  :config
  (setq-default mode-line-format nil)
  (nano-modeline-text-mode t)
  :custom
  (nano-modeline-padding  '(0.0 . 0.0))
  :custom-face
  (nano-modeline-active ((t ( :background nil
                              :underline (:position 10)))))
  (nano-modeline-status ((t ( :underline (:position 10)
                              :foreground ,(face-foreground 'default)))))
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

(require 'nano-modeline)
(set-face-attribute 'vertical-border nil
                    :foreground (face-background 'nano-modeline-active)
                    :strike-through (face-background 'nano-modeline-active)
                    :underline (face-background 'nano-modeline-active)
                    :overline (face-background 'nano-modeline-active))

(use-package spacious-padding
  :ensure t
  :custom  (spacious-padding-widths '( :internal-border-width 25
                                       :header-line-width 30
                                       :mode-line-width 30
                                       :tab-width 4
                                       :right-divider-width 30
                                       :scroll-bar-width 0))
  :config (spacious-padding-mode 1))


(provide 'nano-config)
