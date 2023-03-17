;;; init.el

;; Set the initial directory to start from
(setq default-directory "~/")

;; custom-set-variables that were added by Custom
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Add packages to load path
(add-to-list 'load-path "~/.emacs.d/elpa/")
(add-to-list 'load-path "~/.emacs.d/lisp")


;; Start up into scratch
(setq inhibit-startup-screen t
      backup-inhibited t)

;; Revert files automatically when they are changed on disk
(global-auto-revert-mode t)

;; Disable bell ringing
(setq ring-bell-function 'ignore)


;; Define custom keymap
(setq wh-keymap-prefix-key "C-j")

(global-unset-key (kbd wh-keymap-prefix-key))
(define-prefix-command 'wh-keymap)
(global-set-key (kbd wh-keymap-prefix-key) 'wh-keymap)


;; Replace
;; Replace a regexp with a string
(define-key wh-keymap (kbd "r r") 'replace-regexp)
(define-key wh-keymap (kbd "b r") 'rename-buffer)

;; Fix the Mouse wheel with
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;;Display
(show-paren-mode 1)
(column-number-mode 1)
(set-scroll-bar-mode nil)
;; (set-face-attribute 'default nil :height 130)
;; (set-face-attribute 'default nil :font "Input Mono")
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq-default line-spacing 3)
(setq split-width-threshold 200)
(setq split-height-threshold 200)
                   
;;Spacing
(setq-default indent-tabs-mode nil
              tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Fill
(auto-fill-mode 1)
(setq-default fill-column 80)
(setq-default adaptive-fill-regexp "[ 	]*\\([-–!|#%;>*·•‣⁃◦/]+[ 	]*\\)*")

;; Enable these commands
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Insert a new line when saving a file
(setq require-final-newline t)


;;Package Setup
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(require 'dot)

;;Get rid of tool and menu bars
(and (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(and (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Get rid of all frame decorations
;; Commented out so that amethyst can grab the title bar
;; (add-to-list 'default-frame-alist '(undecorated . t))
(setq ns-auto-hide-menu-bar t)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
(add-to-list 'default-frame-alist '(frame-title-format . nil)) ;; assuming you are using a dark theme
(add-to-list 'default-frame-alist '(icon-title-format . nil)) ;; assuming you are using a dark theme
(add-to-list 'default-frame-alist '(title . " ")) ;; assuming you are using a dark theme
(setq ns-use-proxy-icon nil)
(setq frame-resize-pixelwise t)

;;Mac Environment
(when (memq window-system '(mac ns))
  (setq mac-command-modifier 'super))

;; (when (file-exists-p "~/.emacs.d/project-inits")
;;   (load-dir-one "~/.emacs.d/project-inits"))

(server-start)
;;; init.el ends here
