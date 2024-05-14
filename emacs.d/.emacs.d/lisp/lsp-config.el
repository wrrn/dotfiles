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
         ("i f"   . lsp-find-implementations)
         ("v r"   . lsp-rename))


  :commands lsp)

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
