;; term-config.el --- Configure Terminal specific packages

(use-package vterm
  :custom
  (vterm-min-window-width 20)
  ;; Remove C-j from the vterm-keymap so that it doesn't conflict with
  ;; wh-keymap.
  :bind (:map vterm-mode-map
              ("C-j" . nil))
  :ensure t)


(use-package multi-vterm
  :ensure t
  :requires vterm
  ;; :bind (
  ;;        ("C-x p t"   . multi-vterm-project)
  ;;        :map vterm-mode-map
  ;;        ("C-c r" . multi-vterm-rename-buffer)
  ;;        :map wh-keymap
  ;;        ("t t" . multi-vterm-dedicated-toggle)
  ;;        ("t c" . multi-vterm)
  ;;        ("t n" . multi-vterm-next)
  ;;        ("t p" . multi-vterm-prev)
  ;;        ("p t" . multi-vterm-project))
  )

(use-package eat
  :ensure t
  :custom
  (eat-kill-buffer-on-exit t)
  :bind (
         ("C-x p t"   . eat-project)
         :map eat-semi-char-mode-map
         ("C-j" . nil)
         :map wh-keymap
         ("t c" . eat)
         ("p t" . eat-project)
         )
  )



(provide 'term-config)
