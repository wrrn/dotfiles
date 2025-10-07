;;; nix-config.el  --- configure nix-mode
;; Nix configuration

(use-package nix-ts-mode
  :ensure t
  :mode "\\.nix\\'"
  :init
  (add-to-list 'treesit-language-source-alist '(nix "https://github.com/nix-community/tree-sitter-nix"))
  )

(provide 'nix-config)
