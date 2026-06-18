;;; 1password-config.el  --- configure 1password.el
;; 1password configuration

(use-package auth-source-1password
  :ensure t
  :config
  (setq auth-source-1password-vault "Private")
  (setq auth-source-1password-construct-secret-reference
        (lambda (_backend _type host user port)
          (mapconcat #'identity
                     (list (or port auth-source-1password-vault) host user)
                     "/")))
  (auth-source-1password-enable))



(provide '1password-config)
