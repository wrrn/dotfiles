;; init.el

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
(define-prefix-command 'wh-keymap)
(global-set-key (kbd "C-o") 'wh-keymap)
(define-key wh-keymap (kbd "C-r") 'rename-buffer)

;; IMenu
;; Allows me to jump to functions
(define-key wh-keymap (kbd "C-f") 'imenu)


;; Fix the Mouse wheel with
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)




;;Display
(show-paren-mode 1)
(column-number-mode 1)
(set-scroll-bar-mode nil)
(set-face-attribute 'default nil :height 120)
(set-face-attribute 'default nil :font "Fira Mono")
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq-default line-spacing 3)
(setq split-width-threshold 200)
(setq split-height-threshold 200)
                   
;;Get rid of tool and menu bars
(and (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(and (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;Spacing
(setq-default indent-tabs-mode nil
              tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Fill
(setq-default fill-column 80)


(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;;Package Setup
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")
                           ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(require 'dot)
;;Mac Environment
(when (memq window-system '(mac ns))
  (setq mac-command-modifier 'control))

(when (file-exists-p "~/.emacs.d/project-inits")
  (load-dir-one "~/.emacs.d/project-inits"))

(server-start)
(toggle-frame-fullscreen)
;;; init.el ends here



