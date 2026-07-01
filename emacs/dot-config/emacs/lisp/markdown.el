;;; markdown.el  --- configure markdown mode
;; Markdown configuration
(use-package markdown-modern
  :straight (markdown-modern
             :host github
             :repo "rjprins/markdown-modern"
             :branch "main"))

(provide 'markdown)
