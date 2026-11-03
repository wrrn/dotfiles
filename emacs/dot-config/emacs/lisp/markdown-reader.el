;;; markdown-reader.el --- distraction-free markdown reading  -*- lexical-binding: t; -*-
;;; Commentary:
;; A read-only markdown mode with writeroom, mixed-pitch, and view-mode
;; paging.  centered-cursor-mode is deliberately left off.
;;; Code:

(use-package mixed-pitch
  :ensure t
  :custom (mixed-pitch-set-height t)
  :hook (markdown-modern-mode . mixed-pitch-mode))

(define-derived-mode markdown-reader-mode markdown-view-mode "Markdown-Reader"
  "Major mode for reading Markdown with `writeroom-mode' and `mixed-pitch-mode'."
  (view-mode 1)
  (mixed-pitch-mode 1)
  (writeroom-mode 1))

(define-key markdown-reader-mode-map (kbd "C-v") #'View-scroll-page-forward)
(define-key markdown-reader-mode-map (kbd "M-v") #'View-scroll-page-backward)

(provide 'markdown-reader)
;;; markdown-reader.el ends here
