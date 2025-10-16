;;; env-config.el --- setup the environment in which emacs runs in

(use-package direnv
  :ensure t
  :config (direnv-mode))

(use-package exec-path-from-shell
  ;; Used to get environment variables for mac
  :ensure t
  :init (progn
          (setq-default shell-file-name (getenv "SHELL"))
          (setq-default exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "HOME"))
          (setq-default default-directory (getenv "HOME"))
          (exec-path-from-shell-initialize)))

;; (use-package asdf
;;   :ensure t
;;   :straight (asdf :type git :host github :repo "tabfugnic/asdf.el")
;;   :custom
;;   (asdf-binary "/usr/local/opt/asdf/libexec/bin/asdf")
;;   :config
;;   (asdf-enable))
(provide 'env-config)
;; ;;; env-config.el ends
