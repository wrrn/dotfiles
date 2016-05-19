;; Ensure that package use-package is initialized
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package exec-path-from-shell
  ;; Used to get environment variables for mac
  :ensure t
  :init (progn
          (setq-default default-directory "/Users/warren")
          (exec-path-from-shell-initialize)
          (exec-path-from-shell-copy-env "GOPATH")
          (exec-path-from-shell-copy-env "PATH")))


(use-package diminish
  :ensure t
  )
          

(use-package material-theme
  ;; Dark theme
  :ensure t
  :config (load-theme 'material t))

(use-package ido
  ;; Nice mini-buffer
  :ensure t
  :init (progn
          (require 'ido)
          (setq ido-everywhere t)
          (setq ido-enable-flex-matching t)
          (ido-mode t)))

(use-package ido-ubiquitous
  ;; ido ubiquitously
  :ensure t
  :config (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  ;; Vertical
  :ensure
  :config (ido-vertical-mode 1))

(use-package smex
  ;; M-x with ido-style
  :ensure t
  :config (smex-initialize)
  :bind (("M-x" . smex)))

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
            ;; Open terminal only if one doesn't exist
            (defun wh-ansi-term ()
              (interactive)
              (let ((buffer (get-buffer "*ansi-term*")))
                (if buffer
                    (switch-to-buffer buffer)
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

(use-package tramp-term
  ;; Tramp Terminal
  :ensure t
  :init (progn
          (setq auto-revert-remote-files t))
    
  :config (defalias 'ssh 'tramp-term)
  :commands tramp-term)





(use-package magit
  :ensure t
  :init (progn
          (setq magit-last-seen-setup-instructions "1.4.0")
          (define-key wh-keymap (kbd "g") 'magit-status)))



(use-package powerline
  :ensure t
  :init (progn

          (require 'powerline)
          (setq powerline-default-separator 'utf-8)
          (powerline-default-theme)))




(use-package go-mode
  ;; GO Mode for editing go programs
  :ensure t
  :mode "\\.go\\'"
  :config (let (( gopath (getenv "GOPATH")))
            (require 'go-mode)
            (setq gofmt-command "goimports")
            (add-hook 'before-save-hook #'gofmt-before-save)
            (load-file (concat gopath "/src/golang.org/x/tools/cmd/guru/go-guru.el"))
            (setq go-oracle-command (concat gopath "/bin/oracle"))
            (add-hook 'go-mode-hook (lambda ()
                                      (set (make-local-variable 'compile-command) "go build")
                                      (subword-mode t)))))




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
            (setq org-enforce-todo-dependencies t)
            (setq org-todo-keywords
                  '((sequence "TODO(t)" "|" "DONE(d)")))
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

(use-package org-indent
  :init (setq org-indent-mode t))

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



(provide 'dot)


































