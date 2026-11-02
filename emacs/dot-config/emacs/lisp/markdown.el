;;; markdown.el  --- configure markdown mode
;; Markdown configuration
(use-package markdown-modern
  :ensure true
  :straight (markdown-modern
             :host github
             :repo "rjprins/markdown-modern"
             :branch "main")
  :hook(markdown-modern . mixed-pitch-mode))

(provide 'markdown)
