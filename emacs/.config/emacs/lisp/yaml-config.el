;;; yaml-config.el --- configure yaml mode

(use-package yaml-ts-mode
  :ensure t
  :mode "\\.yml\\'"
  :init
  (add-to-list 'treesit-language-source-alist '(yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

(provide 'yaml-config)
