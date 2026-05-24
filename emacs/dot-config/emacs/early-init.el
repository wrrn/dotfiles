;; Start up into scratch
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
;; (setq ns-auto-hide-menu-bar t)
;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
;; (add-to-list 'default-frame-alist '(frame-title-format . nil)) ;; assuming you are using a dark theme
;; (add-to-list 'default-frame-alist '(icon-title-format . nil)) ;; assuming you are using a dark theme
;; (add-to-list 'default-frame-alist '(title . nil)) ;; assuming you are using a dark theme
;; (setq ns-use-proxy-icon nil)
(setq frame-resize-pixelwise t)
;; (set-face-foreground 'window-divider-first-pixel "gray")
;; (set-face-background 'window-divider-first-pixel "gray")

;; (set-face-foreground 'window-divider "gray")
;; (set-face-background 'window-divider "gray")
