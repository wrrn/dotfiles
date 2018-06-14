;;; dot.el --- load all of my custom packages
;;; Commentary:
;;; Code:
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package exec-path-from-shell
  ;; Used to get environment variables for mac
  :ensure t
  :init (progn
          (setq-default exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "HOME"))
          (setq-default default-directory (getenv "HOME"))
          (exec-path-from-shell-initialize)))

(use-package diminish
  :ensure t
  )


(use-package spacemacs-common
    :ensure spacemacs-theme
    :config (load-theme 'spacemacs-dark t))

(use-package smex
  :ensure t
  :config (smex-initialize))

(use-package ivy
  ;; Interactive interface completion
  :ensure t
  :diminish ivy-mode
  :init (progn
          (setq ivy-use-virtual-buffers t)
          (setq ivy-count-format "(%d/%d) ")
          (setq ivy-sort-file-function 'string-lessp)
          (setq ivy-extra-directories nil)
          (setq magit-completing-read-function 'ivy-completing-read))
  :config (ivy-mode 1)
  :bind (("C-c C-r" . ivy-resume)))



(use-package swiper
  :ensure t
  :bind (("C-s" . counsel-grep-or-swiper)))

(use-package counsel
  :ensure t
  :init  (progn
           (defun counsel-env-res (res path)
             (let ((apath (abbreviate-file-name path)))
               (list (car res)
                     (if (file-accessible-directory-p path)
                         (file-name-as-directory apath)
                       apath))))

           (defun counsel-env ()
             (delq nil
                   (mapcar
                    (lambda (s)
                      (let* ((res (split-string s "=" t))
                             (path (cadr res)))
                        (when (stringp path)
                          (cond ((file-exists-p path)
                                 (counsel-env-res res path))
                                ((file-exists-p (expand-file-name path ivy--directory))
                                 (counsel-env-res
                                  res (expand-file-name path ivy--directory)))
                                (t nil)))))
                    process-environment)))

           (defun counsel-expand-env ()
             (interactive)
             (if (equal ivy-text "")
                 (progn
                   (let ((enable-recursive-minibuffers t)
                         (history (symbol-value (ivy-state-history ivy-last)))
                         (old-last ivy-last)
                         (ivy-recursive-restore nil))
                     (ivy-read "Env: " (counsel-env)
                               :action (lambda (x)
                                         (ivy--reset-state (setq ivy-last old-last))
                                         (setq ivy--directory "")
                                         (delete-minibuffer-contents)
                                         (insert (cadr x))))))
               (insert "$")))
           )
  :bind(("M-x" . counsel-M-x)
        ("C-x C-f" . counsel-find-file)
        ("<f1> f" . counsel-describe-function)
        ("<f1> v" . counsel-describe-variable)
        ("<f1> l" . counsel-find-library)
        ("<f2> i" . counsel-info-lookup-symbol)
        ("<f2> u" . counsel-unicode-char)
        :map counsel-find-file-map
        ("$" . counsel-expand-env)))

(use-package multiple-cursors
  ;; Multiple Cursors for Emacs.
  :ensure t
  :bind (("C-<" . mc/mark-previous-like-this-word)
         ("C-c <" . mc/skip-to-previous-like-this)
         ("C->" . mc/mark-next-like-this-word)
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
            ;; Kill terminal buffer on terminal exit
            (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
              (if (memq (process-status proc) '(signal exit))
                  (let ((buffer (process-buffer proc)))
                    ad-do-it
                    (kill-buffer buffer))
                ad-do-it))
            (ad-activate 'term-sentinel))
  :bind (("C-x t" . wh-ansi-term)))

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
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :bind (:map wh-keymap
              ("g" . magit)))


(use-package smart-mode-line
  :ensure t
  :init (setq sml/theme 'respectful)
  :config (sml/setup))

(use-package ggtags
  :ensure t
  )

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
                                      (subword-mode t)
                                      (message "original go path mode hook")))
            (use-package go-guru
              :ensure t)

            (use-package company-go
              :ensure t
              :requires auto-complete
              :init (progn
                      (require 'company-go)
                      (require 'go-mode)))


            (use-package go-eldoc
              :ensure t
              :requires go-autocomplete
              :init (require 'go-eldoc))

            (use-package ob-go
              :ensure t
              )

            (add-hook 'go-mode-hook (lambda ()
                                      (set (make-local-variable 'company-backends) '(company-go))
                                      (company-mode)
                                      (message "company-go hook")
                                      ))
            (add-hook 'go-mode-hook 'go-eldoc-setup)

            ))

(use-package go-rename
  :ensure t)

(use-package company
  :ensure t
  :config (progn
            (require 'company)
            (setq company-tooltip-limit 20)                      ; bigger popup window
            (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
            (setq company-echo-delay 0)                          ; remove annoying blinking
            (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing))
            (add-hook 'after-init-hook 'global-company-mode) ; Use company mode everywhere


            ))


(use-package org
  :config (progn
            (setq org-startup-indented t)
            (setq org-enforce-todo-dependencies t)
            (setq org-todo-keywords
                  '((sequence "TODO(t)" "IN PROGESS(p)" "IN PEER REVIEW(r)" "|" "DONE(d)")
                    (sequence "QUESTION(q)" "|" "ANSWERED(a)")))
            (setq org-log-done 'time)
            (setq org-enforce-todo-checkbox-dependencies t)
            ;; Recursive count of todos
            (setq org-hierarchical-todo-statistics nil)
            (setq org-checkbox-hierarchical-statistics nil)

            ;; Add languages to code blocks
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((emacs-lisp . t)
               (shell . t)
               (go . t)))

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
            (setq appt-display-duration 30)
            ;; Org to markdown conversion options
            (setq org-export-with-toc nil)
            (setq org-export-headline-levels 5)
            ;; Export org files to github markdown
            (use-package ox-gfm
              :ensure t)
            
            (setq org-export-backends (quote (ascii html icalendar latex md gfm)))))

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

(use-package desktop
  :ensure t
  :bind (:map wh-keymap
              ("C-d c" . desktop-clear)
              ("C-d s" . desktop-save)
              ("C-d d" . desktop-remove)))

(use-package time
  ;; Add time to modebar
  :init (setq display-time-string-forms '(24-hours "." minutes))
  :config (display-time))

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
  :mode "\\.md\\'"
  :config (add-hook 'markdown-mode-hook 'auto-fill-mode))


(use-package nginx-mode
  :ensure t
  :defer t)

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook ((web-mode-hook . rainbow-mode)
         (scss-mode-hook . rainbow-mode)))


(use-package rust-mode
  :ensure t
  :init (setq rust-format-on-save t)
  :mode "\\.rs\\'")

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package feature-mode
  :ensure t
  :config (setq feature-default-lanaguage "en")
  :mode "\\.feature\\'")

(use-package elec-pair
  :ensure t
  :init
  :config (electric-pair-mode))

(use-package simple
  :diminish visual-line-mode
  ;; Nice Line Wrapping
  :init (setq visual-line-mode 80)
  :config (global-visual-line-mode))


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
(use-package rg
  :ensure t)

(use-package zoom-window
  :ensure t
  :init 
  :bind (:map wh-keymap
              ("C-z" . zoom-window-zoom)))

(use-package linum-off
  :ensure t
  :init (defvar linum-disabled-modes-list '(eshell-mode term-mode ivy-mode compilation-mode org-mode text-mode dired-mode pdf-view-mode)))

;; Ansi Color interpretation in the compilation buffer
(use-package ansi-color
  :ensure t
  :init (progn
          (defun colorize-compilation-buffer ()
            (let ((inhibit-read-only t))
              (ansi-color-apply-on-region (point-min) (point-max))))
          (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))

(use-package pickle
  :ensure t
  :mode ("\\.feature\\'" . pickle-mode))

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
;;; dot.el ends here
