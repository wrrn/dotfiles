;;; writeroom-config.el  --- configure writeroom
(use-package centered-cursor-mode
  :ensure t)

(use-package writeroom-mode
  :ensure t
  :requires centered-cursor-mode
  :custom
  (writeroom-maximize-window t)
  (writeroom-fullscreen-effect "maximized")
  :hook
  (writeroom-mode . centered-cursor-mode)
  (writeroom-disable . centered-cursor-mode))

(provide 'writeroom-config)
