;;; docker-config.el --- configure docker mode

(use-package dockerfile-ts-mode
  :ensure t
  :mode
  ("Dockerfile.*\\'" . dockerfile-ts-mode)
  ("\\.Dockerfile\\'" . dockerfile-ts-mode)
  :init
  (add-to-list 'treesit-language-source-alist '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")))

(provide 'docker-config)
