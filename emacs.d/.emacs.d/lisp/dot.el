;; Ensure that package use-package is initialized
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package exec-path-from-shell
  ;; Used to get environment variables for mac
  :ensure t
  :init (progn
          (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "HOME" "PRMUNTY"))
          (setq-default default-directory (getenv "HOME"))
          (exec-path-from-shell-initialize)))

(use-package diminish
  :ensure t
  )
          

(use-package zenburn-theme
  ;; Dark theme
  :ensure t
  :config (load-theme 'zenburn t))

(use-package ivy
  ;; Interactive interface completion
  :ensure t
  :diminish ivy-mode
  :init (progn
          (ivy-mode 1)
          (setq ivy-use-virtual-buffers t)
          (setq ivy-count-format "(%d/%d) ")
          (setq ivy-sort-file-function 'string-lessp)
          (setq ivy-extra-directories nil)
          (setq magit-completing-read-function 'ivy-completing-read)
          
          (global-set-key (kbd "C-c C-r") 'ivy-resume)))



(use-package swiper
  :ensure t
  :init (progn
          (global-set-key (kbd "C-s") 'swiper))
  )

(use-package counsel
  :ensure t
  :init (progn
          (global-set-key (kbd "M-x") 'counsel-M-x)
          (global-set-key (kbd "C-x C-f") 'counsel-find-file)
          (global-set-key (kbd "<f1> f") 'counsel-describe-function)
          (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
          (global-set-key (kbd "<f1> l") 'counsel-find-library)
          (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
          (global-set-key (kbd "<f2> u") 'counsel-unicode-char)))

(use-package multiple-cursors
  ;; Multiple Cursors for Emacs.
  :ensure t
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C-c <" . mc/skip-to-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c >" . mc/skip-to-next-like-this)
         ("C-c C-a" . mc/mark-all-like-this)))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-char-mode)))



(use-package term
	     ;; Terminal
	     :init (add-hook 'term-mode-hook (lambda ()
					       "Remove global-linum-mode to stop flashing and line-spacing to fix less"
					       (linum-mode -1)
					       (set (make-local-variable 'line-spacing) nil)))
  :config (progn
            (term-set-escape-char ?\C-x)
            (defun wh-ansi-term ()
              ;; Open terminal only if one doesn't exist
              (interactive)
              (let ((buffer (get-buffer "*ansi-term*")))
                (if buffer
                    (pop-to-buffer buffer '((display-buffer-reuse-window)
                                            (reusable-frames . t)))
                  (ansi-term (getenv "SHELL")))))
            (define-key global-map (kbd "C-x t") 'wh-ansi-term)
            ;; Kill terminal buffer on terminal exit
            (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
              (if (memq (process-status proc) '(signal exit))
                  (let ((buffer (process-buffer proc)))
                    ad-do-it
                    (kill-buffer buffer))
                ad-do-it))
            (ad-activate 'term-sentinel)))

(use-package tramp
  :init (setq tramp-default-method "ssh"))

(use-package tramp-term
  ;; Tramp Terminal
  :ensure t
  :init (progn
          (setq auto-revert-remote-files t)
          (require 'tramp-term))
    
  :config (defalias 'ssh 'tramp-term)
  :commands tramp-term)





(use-package magit
  :ensure t
  :init (progn
          (setq magit-last-seen-setup-instructions "1.4.0")
          (define-key wh-keymap (kbd "g") 'magit-status)))



(use-package smart-mode-line
  :ensure t
  :init (progn
          (setq sml/theme 'respectful)
          (sml/setup)))


(use-package go-mode
  ;; GO Mode for editing go programs
  :ensure t
  :mode "\\.go\\'"
  :config (let (( gopath (getenv "GOPATH")))
            (require 'go-mode)
            (setq gofmt-command "goimports")
            (add-hook 'before-save-hook #'gofmt-before-save)
            (add-hook 'go-mode-hook (lambda ()
                                      (set (make-local-variable 'compile-command) "go build")
                                      (subword-mode t)))
            (use-package go-guru
              :ensure t)))






(use-package auto-complete
  :ensure t)

(use-package go-autocomplete
  :ensure t
  :requires auto-complete
  :init (progn
          (require 'go-autocomplete)
          (require 'auto-complete-config)
          (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)))


(use-package go-eldoc
  :ensure t
  :requires go-autocomplete
  :init (progn
          (require 'go-eldoc)
          (add-hook 'go-mode-hook 'go-eldoc-setup)))

(use-package org
  :config (progn
            (setq org-startup-indented t)
            (setq org-enforce-todo-dependencies t)
            (setq org-todo-keywords
                  '((sequence "TODO(t)" "IN PROGESS(p!)" "|" "DONE(d)")))
            (setq org-log-done 'time)
            (setq org-enforce-todo-checkbox-dependencies t)
            ;; Recursive count of todos
            (setq org-hierarchical-todo-statistics nil)
            (setq org-checkbox-hierarchical-statistics nil)

            ;; Add languages to code blocks
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((emacs-lisp . t)
               (sh . t)))
            ;; Syntax highlighting in code blocks
            (setq org-src-fontify-natively t)
            
            (add-hook 'org-after-todo-statistics-hook (lambda(n-done n-not-done)
                                                       "Switch entry to DONE when all subentries are done, to TODO otherwise"
                                                       (let (org-log-done org-log-status)
                                                         (org-todo (if (= n-not-done 0)
                                                                       "DONE"
                                                                     (org-get-todo-sequence-head (org-get-todo-state)))))))

            ;; Opens appointment reminders in current window
            (setq appt-display-format 'window)
            (setq appt-display-duration 30)))

(use-package web-mode
  :ensure t
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
          (setq web-mode-engines-alist
                '(("go" . "\\.html\\'")))))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config (add-hook 'js2-mode-hook (lambda()
                                     "Disable js2 toggle hide functions"
                                     (local-unset-key (kbd "C-c C-f"))
                                     (js2-minor-mode))))

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :init (setq scss-sass-command "sass --style=compressed"))


(use-package haskell-mode
  :ensure t
  :config (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(use-package ediff
  :init (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package wgrep
  :ensure t
  :init (require 'wgrep))

(use-package ansible
  :ensure t
  :defer t)

(use-package ansible-doc
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

(use-package nginx-mode
  :ensure t
  :defer t)

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :init (progn
          (add-hook 'web-mode-hook 'rainbow-mode)
          (add-hook 'scss-mode-hook 'rainbow-mode)))

(use-package rust-mode
  :ensure t
  :config (progn
            (setq rust-format-on-save t))
  :mode "\\.rs\\'")

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package elec-pair
  :init (electric-pair-mode))

(use-package simple
  :diminish visual-line-mode
  :init (progn
          ;; Nice Line Wrapping
          (setq visual-line-mode 80)
          (global-visual-line-mode)))


(use-package erc
  :init(progn
         (setq erc-fill-function 'erc-fill-static)
         (setq erc-fill-static-center 22)))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile.*\\'")

(use-package docker-tramp
  :ensure t)

(use-package load-dir
  :ensure t)

(use-package ztree
  :ensure t)

(use-package protobuf-mode
  :ensure t
  :mode ".*\.proto\\'")

(when (memq window-system '(mac ns))
  (use-package frame
    :init (progn
          (defun kill-fullscreen(frame)
            "When a FRAME is deleted, take it out of fullscreen first. This
fixes the bug where emacs dies when you try to kill a frame"
            (modify-frame-parameters
             frame
             `(
               (fullscreen
                . ,(if (eq (frame-parameter frame 'maximized) 'maximized)
                       'maximized)))))
          (add-to-list 'delete-frame-functions #'kill-fullscreen)
          
  )))

(provide 'dot)
