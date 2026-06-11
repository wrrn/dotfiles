;;; writeroom-config.el  --- configure writeroom
(use-package centered-cursor-mode
  :ensure t)

(use-package writeroom-mode
  :ensure t
  :requires centered-cursor-mode
  :custom
  (writeroom-maximize-window t)
  (writeroom-fullscreen-effect "maximized")
  (writeroom-major-modes '(prog-mode ghostel-mode text-mode))
  :hook
  (writeroom-mode . maybe-centered-cursor-mode)
  (writeroom-disable . maybe-centered-cursor-mode))

(defun maybe-centered-cursor-mode ()
  "Toggle `centered-cursor-mode' unless in `ghostel-mode'."
  (unless (derived-mode-p 'ghostel-mode)
    (centered-cursor-mode)))

(provide 'writeroom-config)
