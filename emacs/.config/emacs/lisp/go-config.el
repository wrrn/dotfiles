;;; go-config.el  --- configure meow mode
;; Go configuration

(defun go-mode--auto-fill-comments ()
  "Autofill only the comments in go files"
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun go-mode--set-compile-command ()
  "Set the compile to command to 'go test' for test files otherwise use 'go build'"
  (set (make-local-variable 'compile-command)
       (if (string-suffix-p "_test.go" (buffer-file-name))
           "go test"
         "go build"))
  (subword-mode t))

(use-package go-ts-mode
  ;; GO Mode for editing go programs
  :ensure t
  :mode "\\.go\\'"
  :hook ((go-ts-mode . lsp-deferred)
         (go-ts-mode . go-mode--auto-fill-comments)
         (go-ts-mode . go-mode--set-compile-command))
  :init
  (add-to-list 'treesit-language-source-alist '(go "https://github.com/tree-sitter/tree-sitter-go"))
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
  :config
  (add-to-list 'lsp-language-id-configuration '(go-ts-mode . "go")))

(use-package go-mod-ts-mode
  :ensure nil
  :straight nil
  :after go-ts-mode
  :mode "/go\\.mod\\'"
  :init
  (add-to-list 'treesit-language-source-alist '(gomod "https://github.com/camdencheek/tree-sitter-go-mod")))

(use-package ob-go
  :ensure t
  :init
  (append org-babel-load-languages '((go . t))))

(use-package go-rename
  :ensure t)

(require 'dap-dlv-go)

(provide 'go-config)
