;;; lua-config.el --- configure lua mode

(use-package lua-ts-mode
  :ensure t
  :mode "\\.lua\\'"
  :init
  (add-to-list 'treesit-language-source-alist
               '(lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")))



(provide 'lua-config)
