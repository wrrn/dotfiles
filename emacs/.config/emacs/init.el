;;; init.el

;; Set the initial directory to start from
(setq default-directory "~/")
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)


;; custom-set-variables that were added by Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Add packages to load path
(add-to-list 'load-path (expand-file-name "elpa" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))



;; Revert files automatically when they are changed on disk
(global-auto-revert-mode t)



;; Define custom keymap
(setq wh-keymap-prefix-key "C-j")
(keymap-unset lisp-interaction-mode-map "C-j" t)
(global-unset-key (kbd wh-keymap-prefix-key))
(define-prefix-command 'wh-keymap)
(global-set-key (kbd wh-keymap-prefix-key) 'wh-keymap)

(global-unset-key (kbd "C-l"))
(global-set-key (kbd "C-l") 'wh-keymap)



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

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
;; (setq-default line-spacing 1)
(setq split-width-threshold 160)
(setq split-height-threshold 80)

;;Spacing
(setq-default indent-tabs-mode nil
              tab-width 2)
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
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(require 'dot)

;;Mac Environment
(when (memq window-system '(mac ns))
  (setq mac-command-modifier 'super))

;; (when (file-exists-p "~/.emacs.d/project-inits")
;;   (load-dir-one "~/.emacs.d/project-inits"))

;; (server-start)
;;; init.el ends here
