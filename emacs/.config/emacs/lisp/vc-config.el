;;; vc-config.el --- configure version control packages
(use-package magit
  :ensure t
  :init (progn
          (setq magit-last-seen-setup-instructions "1.4.0")
          (setq magit-diff-refine-hunk t))
  :bind (:map wh-keymap
              ("g s" . magit)
              ("g d" . magit-diff-range)))

(use-package majutsu
  :ensure t
  :straight (:host github :repo "0WD0/majutsu")
  :bind (:map wh-keymap
              ("j j" . majutsu-log)))

(use-package vc-jj
  :ensure t)

(use-package jj-link
  :straight (:host sr.ht :repo "~warren/jj-link"
                   :files ("*.el"))
  :bind (:map wh-keymap
              ("j l" . jj-link)
              ("j c" . jj-link-commit)
              ("j h" . jj-link-homepage)))

(provide 'vc-config)
