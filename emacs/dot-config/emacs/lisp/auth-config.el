(use-package auth-source-1password -*- lexical-binding: t; -*-
  :ensure t
  :config
  (setq auth-source-1password-vault "Engineering")
  (auth-source-1password-enable))
