;;; dot.el --- load all of my custom packages
;;; Commentary:
;;; Code:
(defvar straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(require 'use-package)



;; (use-package color-theme-sanityinc-tomorrow
;;     :ensure t
;;     :config (load-theme 'sanityinc-tomorrow-night t))

(use-package nano
  :straight (nano :type git :host github :repo "rougier/nano-emacs"
                  :fork (:host github
                               :repo "wrrn/nano-emacs")
                  :no-byte-compile t))

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

(use-package org
  :custom
  (org-startup-indented t) ; cleaner looking org-mode
  (org-tags-column 80) ; calling org-align-all-tags puts all the tags on line 80
  (org-startup-with-inline-images t) ; Show images inline any time there is a link to an image
  (org-enforce-todo-dependencies t) ;; Force everything to DONE before marking a parent done.
  (org-hide-emphasis-markers t) ; Hide the emphasis markers for bold, strike-through, italic, underlined, verbatim, and code
  (org-todo-keywords
        '((sequence "IN PROGRESS(p)" "In Peer Review(r)" "Waiting(w)" "HOLD(h)" "TODO(t)" "|" "DONE(d)")
          (sequence "QUESTION(q)" "|" "ANSWERED(a)")
          (sequence "|" "NOT DOING(n)")))
  (org-todo-keyword-faces
        '(("Waiting" . org-warning)
          ("HOLD" . org-warning)
          ("In Peer Review" . org-warning)
          ("IN PROGRESS" . (:foreground "#f1fa8c" :bold t :background "#373844"))))
  (org-log-done 'time) ;; Log when something was marked as done
  (org-fontify-done-headline t) ;; Allow strike throughs for DONE items
  (org-enforce-todo-checkbox-dependencies t) ;; Force checkboxes to be a dependency before moving TODO's to DONE
  (org-hierarchical-todo-statistics nil) ;; Recursive count of todos
  (org-checkbox-hierarchical-statistics nil) ;; Recursive count of todos
  (org-src-fontify-natively t) ;; Syntax highlighting in code blocks
  (appt-display-format 'window)   ;; Opens appointment reminders in current window
  (appt-display-duration 30) ;; Display the appointment reminder for 30 seconds

  (org-export-with-toc nil)   ;; Org to markdown conversion options
  (org-export-headline-levels 5)
  (org-agenda-files (list (concat (getenv "HOME") "/.roam")))

  :config (progn

            ;; Strike through DONE
            (set-face-attribute 'org-done nil :strike-through t)
            (set-face-attribute 'org-headline-done nil
                                :strike-through t)

            ;; Add languages to code blocks
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((emacs-lisp . t)
               (js . t)
               (org . t)
               (python . t)
               (shell . t)
               (sql . t)))
            (add-hook 'org-after-todo-statistics-hook (lambda(n-done n-not-done)
                                                        "Switch entry to DONE when all subentries are done, to TODO otherwise"
                                                        (let (org-log-done org-log-status)
                                                          (org-todo (if (= n-not-done 0)
                                                                        "DONE"
                                                                      (org-get-todo-sequence-head (org-get-todo-state)))))))
            ;; Export org files to github markdown
            (use-package ox-gfm
              :ensure t)

            (setq org-export-backends (quote (ascii html icalendar latex md gfm)))))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package ob-mermaid
  :ensure t
  :custom
  (ob-mermaid-cli-path (concat (getenv "HOME") "/node_modules/.bin/mmdc"))
  :init
  (append org-babel-load-languages '((go . t))))

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory (concat (getenv "HOME") "/.roam"))
  (org-roam-completion-system 'ivy)
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ;; ("C-c n j" . org-roam-jump-to-index)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

;; Deft helps me look up org-roam files quickly. Like rg or grep but using ivy on the fly
(use-package deft
  :ensure t
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory (concat (getenv "HOME") "/.roam")))

(use-package org-journal
  :ensure t
  :bind (:map wh-keymap
              ("j o" . org-journal-open-current-journal-file)
              ("j e" . org-journal-new-entry))
  :custom
  (org-journal-file-header "#+TITLE: %B %Y")
  (org-journal-file-format "%Y-%m.org")
  (org-journal-dir (concat (getenv "HOME") "/.roam"))
  (org-journal-date-format "%A, %F")
  (org-journal-time-format "")
  (org-journal-file-type 'monthly)
  (org-journal-carryover-items "/!"))

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
          (setq magit-completing-read-function 'ivy-completing-read)
          (setq ivy-display-style 'fancy)
          (ivy-mode 1))
  :bind (("C-c C-r" . ivy-resume)))

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
        ("C-x b" . counsel-switch-buffer)
        ("C-x 4 b" . counsel-switch-buffer-other-window)

        :map counsel-find-file-map
        ("$" . counsel-expand-env)
        
        :map wh-keymap
        ("h f" . counsel-describe-function)
        ("h v" . counsel-describe-variable)
        ("h l" . counsel-find-library)
        ("h i" . counsel-info-lookup-symbol)
        ("h u" . counsel-unicode-char)
        ("y" . counsel-yank-pop)))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)))

(use-package multiple-cursors
  ;; Multiple Cursors for Emacs.
  :ensure t
  :bind (("C-<" . mc/mark-previous-like-this-word)
         ("C-c <" . mc/skip-to-previous-like-this)
         ("C->" . mc/mark-next-like-this-word)
         ("C-c >" . mc/skip-to-next-like-this)
         ("C-c C-a" . mc/mark-all-like-this)))

(use-package avy
  :ensure t
  :bind (("C-c SPC" . avy-goto-char)
         ("C-c C-SPC w" . avy-goto-word-0 )
         ("C-c C-SPC c l" . avy-copy-line)
         ("C-c C-SPC c r" . avy-copy-region)
         ("C-c C-SPC k l" . avy-kill-whole-line)
         ("C-c C-SPC k r" . avy-kill-region)))
  


(use-package vterm
  :ensure t)

(use-package multi-vterm
  :ensure t
  :requires vterm
  :bind (("C-c C-c" . vterm-send-C-c)
         :map wh-keymap
               ("t t" . multi-vterm-dedicated-toggle)
               ("t c" . multi-vterm)
               ("t n" . multi-vterm-next)
               ("t p" . multi-vterm-prev)
        ))

(use-package tramp
  :init   (setq tramp-default-method "ssh")
  :config (progn (add-to-list 'tramp-default-proxies-alist
                              '(nil "\\`root\\'" "/ssh:%h:"))
                 (add-to-list 'tramp-default-proxies-alist
                              '((regexp-quote (system-name)) nil nil))))

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
          (setq magit-diff-refine-hunk t))
  :bind (:map wh-keymap
              ("g g" . magit)
              ("g d" . magit-diff-range)))
  
(use-package ggtags
  :ensure t
  )

(use-package go-mode
  ;; GO Mode for editing go programs
  :ensure t
  :mode "\\.go\\'"
  :bind (
         :map go-mode-map
              ;; Explicitly call lsp-find-references until 
              ("M-?"       . lsp-find-references)
              ("C-c C-d"   . lsp-describe-thing-at-point)
          )
  :config (let (( gopath (getenv "GOPATH")))
            (require 'go-mode)
            (setq gofmt-command "goimports")
            (add-hook 'before-save-hook #'gofmt-before-save)
            ;; auto-fill comments only
            (add-hook 'go-mode-hook (lambda () (set (make-local-variable 'comment-auto-fill-only-comments) t)))
            (add-hook 'go-mode-hook (lambda ()
                                      (set (make-local-variable 'compile-command)
                                           (if (string-suffix-p "_test.go" (buffer-file-name))
                                               "go test"
                                             "go build"))
                                      (subword-mode t)))
                      
            (use-package ob-go
              :ensure t
              :init
              (append org-babel-load-languages '((go . t))))

            (add-hook 'go-mode-hook #'lsp)

            ))

(use-package go-rename
  :ensure t)

(use-package go-dlv
  :ensure t)


(use-package lsp-mode
  :ensure t
  :commands lsp
  :bind (("C-<tab>"   . completion-at-point)
         ("C-?"       . lsp-find-references)
         )
  :custom
  ;; Disable the breadcrumbs in the headerline.
  (lsp-headerline-breadcrumb-enable nil))

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
              ("C-d r" . desktop-read)
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

;; Editing gherkin files (.feature files)
;; BDD
(use-package feature-mode
  :ensure t
  :config (setq feature-default-lanaguage "en")
  :mode "\\.feature\\'")

;; jsonnet is a superset of json
(use-package jsonnet-mode
  :ensure t)

;; Automatically create closing parens, braces, and quotes
(use-package elec-pair
  :ensure t
  :init
  :config (electric-pair-mode))

;; IMenu
;; Allows me to jump to functions
(use-package imenu
  :ensure t
  :bind (:map wh-keymap
              ("C-f" . imenu)))

(use-package simple
  :straight f
  :diminish visual-line-mode
  ;; Nice Line Wrapping
  :init (setq visual-line-mode 80)
  :config (global-visual-line-mode))

;; Make a small center margin
(use-package fringe
  :straight f
  :init (fringe-mode 1))

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
  :ensure t
  :init  (progn
           (setq-default rg-command-line-flags '("--sort path"))))

(use-package ace-window
  :ensure t
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . ace-window)
         (:map wh-keymap
               ("w" . ace-window))))

(use-package zoom-window
  :ensure t
  :bind (:map wh-keymap
              ("C-z" . zoom-window-zoom)))

;; Show emojis in emacs
(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode))

(use-package linum-off
  :ensure t
  :init (defvar linum-disabled-modes-list '(eshell-mode term-mode ivy-mode compilation-mode org-mode text-mode dired-mode pdf-view-mode)))

 (use-package flyspell
   :ensure t
   :hook ((text-mode . flyspell-mode)
          (prog-mode . flyspell-prog-mode)))

;; Ansi Color interpretation in the compilation buffer
(use-package ansi-color
  :ensure t
  :init (progn
          (defun colorize-compilation-buffer ()
            (let ((inhibit-read-only t))
              (ansi-color-apply-on-region (point-min) (point-max))))
          (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))

(when (memq window-system '(mac ns))
  (use-package frame
    :straight f
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

(use-package beacon
  :ensure t
  :config (progn
            (require 'beacon)
            (beacon-mode +1)))

(use-package re-builder
  :custom
  (reb-re-syntax 'string))

(use-package compile
  :bind (:map wh-keymap
              ("c" . compile)))

(provide 'dot)
;;; dot.el ends here
