;;; json-config.el --- configure json mode

(use-package json-ts-mode
  :ensure t
  :mode "\\.json\\'"
  :init
  (add-to-list 'treesit-language-source-alist '(json "https://github.com/tree-sitter/tree-sitter-json")))

(provide 'json-config)
