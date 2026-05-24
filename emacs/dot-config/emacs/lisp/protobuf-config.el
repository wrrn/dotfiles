;;; protobuf-config.el  --- configure protobuf editing packages

(use-package protobuf-ts-mode
  :ensure t
  :mode ".*\.proto\\'"
  :init
  (add-to-list 'treesit-language-source-alist
               '(proto "https://github.com/mitchellh/tree-sitter-proto")))

(provide 'protobuf-config)


