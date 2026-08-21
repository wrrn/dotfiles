;;; pi-config.el  --- configure pi-coding-agent
;; Configuration

(use-package pi-coding-agent
  :ensure t
  :bind ("C-c p" . pi-coding-agent-toggle)
  )

(provide 'pi-config)
