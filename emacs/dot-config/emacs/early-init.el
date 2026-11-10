;; Start up into scratch  -*- lexical-binding: t; -*-
(setq inhibit-startup-screen t
      backup-inhibited t)

;; Disable bell ringing
(setq ring-bell-function 'ignore)

;; Display
(set-scroll-bar-mode nil)

;; Remove frame decorations
(setq default-frame-alist '((horizontal-scroll-bars . nil)
                            (vertical-scroll-bars . nil)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0) ;; <----- here
                            (internal-border-width . 25)
                            (height . 50)
                            (width . 95)))

(if (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(undecorated-round . t))
  (add-to-list 'default-frame-alist '(undecorated . t)))

(setq frame-resize-pixelwise t)
