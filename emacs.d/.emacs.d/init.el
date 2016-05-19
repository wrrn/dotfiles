; Add ~/.emacs.d to load-path
;;(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/elpa/")
(make-directory "~/.emacs.d/github.com-packages/" t)
(let ((default-directory "~/.emacs.d/github.com-packages/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "~/.emacs.d/lisp")



;; Behaviour
(setq inhibit-startup-screen t
      backup-inhibited t)
(global-auto-revert-mode t)
(setq ring-bell-function 'ignore)


;; Define custom keymap
(define-prefix-command 'wh-keymap)
(global-set-key (kbd "C-o") 'wh-keymap)
(define-key wh-keymap (kbd "C-r") 'rename-buffer)
;; IMenu
;; Allows me to jump to functions
(define-key wh-keymap (kbd "C-f") 'imenu)




;;Display
(show-paren-mode 1)
(column-number-mode 1)
(set-scroll-bar-mode nil)
(set-face-attribute 'default nil :height 120)
(set-face-attribute 'default nil :font "Source Code Pro")
(setq-default line-spacing 3)
(toggle-frame-fullscreen)

                   
;;;;Get rid of tool and menu bars
(and (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(and (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;Spacing
(setq-default indent-tabs-mode nil
              tab-width 4)





;; Frames
(setq pop-up-windows t)

;; Add time to modebar
(setq display-time-string-forms '(24-hours "." minutes))
(display-time)

;; Desktop
(global-set-key (kbd "C-c C-d c") 'desktop-clear)
(global-set-key (kbd "C-c C-d s") 'desktop-save)
(global-set-key (kbd "C-c C-d r") 'desktop-remove) 

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;;Package Setup
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(require 'dot)
;;Mac Environment
(when (memq window-system '(mac ns))
  (setq mac-command-modifier 'control))


(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-safe-themes
   (quote
    ("cb978b7187ea7ac2a3e6bb614d24988301cb5c2c9d1f930dce117792b21ea135" "667e296942c561382fe0a8584c26be0fe7a80416270c3beede8c6d69f2f77ccc" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "fc0c179ce77997ecb6a7833310587131f319006ef2f630c5a1fec1a9307bff45" "97f9438943105a17eeca9f1a1c4c946765e364957749e83047d6ee337b5c0a73" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "51867fa64534ff7ca87fdc1537fbfffc168fa4673e3980850436dc87e31ef426" "d7088a7105aa09cc68e3d058f89917e07e0505e0f4ab522a6045ec8092d67c44" "ad24ea739f229477ea348af968634cb7a0748c9015110a777c8effeddfa920f5" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "c930c2865c39135018cfd3b00bc3971c9990e1bef467558f195c3f036cc506de" "b06aaf5cefc4043ba018ca497a9414141341cb5a2152db84a9a80020d35644d1" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default)))
 '(hl-sexp-background-color "#1c1f26"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
