;;; claude.el --- configure claude-code.el -*- lexical-binding: t; -*-

(use-package transient
  :ensure t)

(use-package claude-code
  :straight (
             :type git
             :host github :repo "stevemolitor/claude-code.el"
             :branch "main"
             :files ("*.el" (:exclude "demo.gif")))
  :bind-keymap
  ("C-j a" . claude-code-command-map)
  :hook ((claude-code--start . sm-setup-claude-faces))
  :config
  (claude-code-mode))


(provide 'claude)
