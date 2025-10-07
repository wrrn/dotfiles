;;; git-config.el --- configure toml mode

(use-package magit
  :ensure t
  :init (progn
          (setq magit-last-seen-setup-instructions "1.4.0")
          (setq magit-diff-refine-hunk t))
  :bind (:map wh-keymap
              ("g s" . magit)
              ("g d" . magit-diff-range)))

(use-package git-link
  :ensure t
  :config (progn
            (add-to-list 'git-link-remote-alist '(git-link-github)))
  :bind ("C-c g l" . git-link))


(provide 'git-config)
