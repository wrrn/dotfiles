;;; typescript.el  --- configure type script
;; Enable typescript editing and indentation

;; TODO
;; 1. Figure out why SPC x f sometimes results C-x C-f.
;; 2. ESC in vterm should drop into v-term-copy-mode
;; 3. Enable meow mode in the minibuffer

(use-package typescript-ts-mode
  :ensure t
  ;; :requires (tree-sitter eglot)
  :mode (("\\.ts\\'"  . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (setq treesit-language-source-alist
        (append treesit-language-source-alist
                '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src" nil nil)
                  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src" nil nil))))
  ;; (treesit-install-language-grammar 'typescript)
  ;; (treesit-install-language-grammar 'tsx)
  ;; ;; ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  ;; (define-derived-mode typescriptreact-mode typescript-mode
  ;;   "TypeScript TSX")

  ;; ;; use our derived mode for tsx files
  ;; (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx))
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)))

;; https://github.com/orzechowskid/tsi.el/
;; great tree-sitter-based indentation for typescript/tsx, css, json
;; (use-package tsi
;;   :ensure t
;;   :after tree-sitter
;;   :straight (tsi :type git :host github :repo "orzechowskid/tsi.el")
;;     ;; :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
;;   ;; define autoload definitions which when actually invoked will cause package to be loaded
;;   :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
;;   :init
;;   (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
;;   (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
;;   (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
;;   (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))


(provide 'typescript)
