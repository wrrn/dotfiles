;;; scala-development.el  --- load all the scala development packages
;; Enable scala-mode for highlighting, indentation and motion commands

;; Enable scala mode
(use-package scala-mode
  :ensure t
  :mode "\\.sc\\'"
  :interpreter
  ("scala" . scala-mode)
  :hook (scala-mode . eglot-ensure))



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
  :custom
  (sbt:prefer-nested-projects t)
  (sbt:program-name "/usr/local/bin/sbt")
)

;; ;; Add metals backend for lsp-mode
;; (use-package lsp-metals
;;   :ensure t
;;   :custom
;;   (lsp-metals-server-args '("-Dmetals.sbt-script=/usr/local/bin/sbt")))

;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe
  :ensure t)

;; DAP is used for debugging code
;; (use-package dap-mode
;;   :ensure t
;;   :hook
;;   (lsp-mode . dap-mode)
;;   (lsp-mode . dap-ui-mode)
;;   )

(provide 'scala-development)
