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
(use-package go-mode
  ;; GO Mode for editing go programs
  :ensure t
  :mode "\\.go\\'"
  :custom
  (gofmt-command "goimports")
  :hook ((go-mode . lsp)
         (go-mode . go-mode--auto-fill-comments)
         (go-mode . go-mode--set-compile-command)
         (before-save . gofmt-before-save )))
(use-package ob-go
  :ensure t
  :init
  (append org-babel-load-languages '((go . t))))

(use-package go-rename
  :ensure t)

(use-package go-dlv
  :ensure t)

(require 'dap-dlv-go)

(provide 'go-config)
