;;; meow-ghostel.el --- meow-mode integration for ghostel -*- lexical-binding: t; -*-

;; Integration between meow modal editing and ghostel's input modes.
;; Ghostel starts in semi-char mode (like insert), so meow also
;; starts in insert state.  <escape> in semi-char switches to
;; ghostel-emacs-mode and meow normal state.

(declare-function ghostel-emacs-mode "ghostel")

(defun meow--ghostel-emacs-mode ()
  "Switch ghostel to Emacs mode and exit meow insert state."
  (interactive)
  (ghostel-emacs-mode)
  (meow-insert-exit))

(provide 'meow-ghostel)
