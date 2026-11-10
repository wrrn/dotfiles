;;; python-config.el  --- configure meow mode -*- lexical-binding: t; -*-
;; Go configuration

(use-package python-ts-mode
  :straight nil
  :mode "\\.py\\'"
  :hook ((python-ts-mode . eglot-ensure))
  :init
  (add-to-list 'treesit-language-source-alist
               '(python "https://github.com/tree-sitter/tree-sitter-python"))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(python-ts-mode . python))
  ;; Eglot tries pylsp before pyright; use pyright wherever it runs.
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((python-mode python-ts-mode)
                   . ("pyright-langserver" "--stdio")))))

(provide 'python-config)
