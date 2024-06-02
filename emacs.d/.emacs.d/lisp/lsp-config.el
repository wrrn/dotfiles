;; lsp-config.el --- configuring lsp since 2024

(use-package lsp-mode
  :ensure t
  :init
  ;; (setq lsp-keymap-prefix "C-l")
  :hook (;; Enable which-key integration         
         (lsp-mode . lsp-enable-which-key-integration))
  :bind (
         :map wh-keymap
         
         ("r f"   . xref-find-references)
         ("d f"   . xref-find-definitions)
         ("4 d f" . xref-find-definitions-other-window)
         ("s f"   . xref-find-apropos)
         ("l l"   . xref-go-back)
         ("t h"   . display-local-help)
         ("c a"   . lsp-execute-code-action)
         ("i f"   . lsp-find-implementation)
         ("v r"   . lsp-rename))


  :commands lsp)

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  ;; Enable tree sitter highlighting
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package ggtags
  :ensure t
  )


;; completion-at-point improvements
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))


;; Language server
;; (use-package eglot
;;   :ensure t
;;   :bind (
;;          :map wh-keymap
;;               ("r f"   . xref-find-references)
;;               ("c a"   . eglot-code-actions)
;;               ("t d"   . display-local-help)
;;               ("d f"   . xref-find-definitions)
;;               ("i f"   . eglot-find-implementation)
;;               ("v r"   . eglot-rename)
;;               ("4 d f" . xref-find-definitions-other-window)
;;               ("s f"   . xref-find-apropos)))


(provide 'lsp-config)
