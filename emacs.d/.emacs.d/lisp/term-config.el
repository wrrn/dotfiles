;; term-config.el --- Configure Terminal specific packages

(use-package vterm
  ;; :config
  ;; ;; Remove C-j from the vterm-keymap so that it doesn't conflict with wh-keymap.
  ;; (customize-set-variable 'vterm-keymap-exceptions (add-to-list 'vterm-keymap-exceptions wh-keymap-prefix-key))
  :bind (:map vterm-mode-map
              ("C-j" . nil))
  ;; :hook
  ;; (vterm-copy-mode . meow-insert-exit)
  :ensure t)


(use-package multi-vterm
  :ensure t
  :requires vterm
  :bind (;; ("C-c C-c" . vterm-send-C-c)
         ("C-x p t"   . multi-vterm-project)
         :map vterm-mode-map
         ("C-c r" . multi-vterm-rename-buffer)
         :map wh-keymap
         ("t t" . multi-vterm-dedicated-toggle)
         ("t c" . multi-vterm)
         ("t n" . multi-vterm-next)
         ("t p" . multi-vterm-prev)
         ("p t" . multi-vterm-project)
         ))

(provide 'term-config)
