;;; markdown.el  --- configure markdown mode  -*- lexical-binding: t; -*-
;; Markdown configuration

(use-package markdown-mode
  :ensure t
  :config (add-hook 'markdown-mode-hook 'auto-fill-mode))


(use-package markdown-modern
  :ensure true
  :straight (markdown-modern
             :host github
             :repo "rjprins/markdown-modern"
             :branch "main")
  :mode "\\.md\\'"
  :hook (markdown-modern . mixed-pitch-mode))

(provide 'markdown)
