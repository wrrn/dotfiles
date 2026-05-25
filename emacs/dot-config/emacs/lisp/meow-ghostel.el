;;; meow-ghostel.el --- meow-mode integration for ghostel -*- lexical-binding: t; -*-

;; Ghostel starts in semi-char mode and meow starts in insert state.
;; Meow's insert-state keymap binds <escape> to `meow-insert-exit',
;; which shadows any per-mode <escape> binding we add to
;; `ghostel-semi-char-mode-map'.  Instead of fighting that, advise
;; `meow-insert-exit' so that exiting insert in a ghostel semi-char
;; buffer also flips ghostel into Emacs mode — keeping ghostel's
;; input-mode in lockstep with meow's editing state.

(declare-function ghostel-emacs-mode "ghostel")

(defun meow--ghostel-sync-emacs-mode (&rest _)
  (when (and (eq major-mode 'ghostel-mode)
             (eq ghostel--input-mode 'semi-char))
    (ghostel-emacs-mode)))

(advice-add 'meow-insert-exit :after #'meow--ghostel-sync-emacs-mode)

(provide 'meow-ghostel)
