;;; claude.el --- configure claude-code.el

(use-package transient
  :ensure t)

(use-package claude-code
  :straight (
             :type git
             :host github :repo "stevemolitor/claude-code.el"
             :branch "main"
             :files ("*.el" (:exclude "demo.gif")))
  :bind-keymap
  ("C-c c" . claude-code-command-map)
  :hook ((claude-code--start . sm-setup-claude-faces))
  :config
  (claude-code-mode))


(provide 'claude)
