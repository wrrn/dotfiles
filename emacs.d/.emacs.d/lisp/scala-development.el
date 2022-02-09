;;; scala-development.el  --- load all the scala development packages
;; Enable scala-mode for highlighting, indentation and motion commands

(use-package scala-mode
  :ensure t
  :interpreter
  ("scala" . scala-mode)
  :hook (scala-mode . lsp)
  (scala-mode . lsp-lens-mode)
  (scala-mode . company-mode))



;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

;; Add metals backend for lsp-mode
(use-package lsp-metals
  :ensure t)


(use-package posframe
  :ensure t
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )
(use-package dap-mode
  :ensure t
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  )

(provide 'scala-development)
