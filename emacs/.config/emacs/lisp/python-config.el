;;; python-config.el  --- configure meow mode
;; Go configuration

(use-package python-ts-mode
  :straight nil
  :mode "\\.py\\'"
  :hook ((python-ts-mode . lsp-deferred))
  :init
  (add-to-list 'treesit-language-source-alist
               '(python "https://github.com/tree-sitter/tree-sitter-python"))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(python-ts-mode . python))
  (add-to-list 'lsp-language-id-configuration '(python-ts-mode . "python")))

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright")
  :hook ((python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp)))
         (python-ts-mode . (lambda ()
                             (require 'lsp-pyright)
                             (lsp)))

         ))

(provide 'python-config)
