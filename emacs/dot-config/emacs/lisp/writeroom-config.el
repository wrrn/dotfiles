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
  (writeroom-mode . maybe-centered-cursor-mode))

(defun maybe-centered-cursor-mode ()
  "Match `centered-cursor-mode' to `writeroom-mode'.
Skip `ghostel-mode' and `markdown-reader-mode'."
  (unless (derived-mode-p 'ghostel-mode 'markdown-reader-mode)
    (centered-cursor-mode (if writeroom-mode 1 -1))))

(provide 'writeroom-config)
