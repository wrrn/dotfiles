;; line-wrap.el --- Configure the line wrapping

(use-package simple
  :straight f
  :diminish visual-line-mode
  ;; Nice Line Wrapping
  :init (setq visual-line-mode 80)
  :config (global-visual-line-mode))


(use-package adaptive-wrap
  :straight (adaptive-wrap
             :type git
             :host github
             :repo "emacs-straight/adaptive-wrap"
             :fork (:host sourcehut
                          :protocol ssh
                          :repo "~warren/adaptive-wrap"))
  :hook (text-mode . adaptive-wrap-prefix-mode)
  
  :custom
  (adaptive-wrap-extra-indent 2))

(provide 'line-wrap)
