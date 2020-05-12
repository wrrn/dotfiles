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


(use-package flucui-themes
    :ensure t
    :config (load-theme 'flucui-light t))

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
          (ivy-mode 1))
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
              ("g" . magit)))

(use-package spaceline
  :ensure t
  :init (progn
          (require 'spaceline-config)
          (spaceline-spacemacs-theme)))
  
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
              :ensure t)

            (add-hook 'go-mode-hook 'go-eldoc-setup)
            (add-hook 'go-mode-hook #'lsp)

            ))

(use-package go-rename
  :ensure t)


(use-package lsp-mode
  :ensure t
  :commands lsp
  :bind (("C-<tab>"   . completion-at-point)
         ("C-?"       . lsp-find-references)
         ))

(use-package org
  :config (progn
            (setq org-startup-indented t) ; cleaner looking org-mode
            (setq org-tags-column 80) ; calling org-align-all-tags puts all the tags on line 80
            (setq org-startup-with-inline-images t) ; Show images inline any time there is a link to an image
            (setq org-enforce-todo-dependencies t) ;; Force everything to DONE before marking a parent done.
            (setq org-hide-emphasis-markers t) ; Hide the emphasis markers for bold, strike-through, italic, underlined, verbatim, and code
            (setq org-todo-keywords
                  '((sequence "TODO(t)" "IN PROGRESS(p)" "In Peer Review(r)" "Waiting(w)" "HOLD(h)" "|" "DONE(d)")
                    (sequence "QUESTION(q)" "|" "ANSWERED(a)")
                    (sequence "|" "NOT DOING(n)")))
            (setq org-todo-keyword-faces
                  '(("Waiting" . org-warning)
                    ("HOLD" . org-warning)
                    ("In Peer Review" . org-warning)
                    ("IN PROGRESS" . (:foreground "#f1fa8c" :bold t :background "#373844"))))
            (setq org-log-done 'time)
            (setq org-fontify-done-headline t)
            (set-face-attribute 'org-done nil :strike-through t)
            (set-face-attribute 'org-headline-done nil
                      :strike-through t)
            (setq org-enforce-todo-checkbox-dependencies t)
            ;; Recursive count of todos
            (setq org-hierarchical-todo-statistics nil)
            (setq org-checkbox-hierarchical-statistics nil)

            ;; Add languages to code blocks
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((emacs-lisp . t)
               (go . t)
               (js . t)
               (org . t)
               (python . t)
               (shell . t)
               (sql . t)))

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

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

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
               ("C-c n j" . org-roam-jump-to-index)
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
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir (concat (getenv "HOME") "/.roam"))
  (org-journal-date-format "%A, %d %B %Y"))

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
  :diminish visual-line-mode
  ;; Nice Line Wrapping
  :init (setq visual-line-mode 80)
  :config (global-visual-line-mode))

;; Make a small center margin
(use-package fringe
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
           (setq-default rg-executable (concat (getenv "HOME") "/.cargo/bin/rg"))
           (setq-default rg-command-line-flags '("--sort path"))))
(use-package zoom-window
  :ensure t
  :bind (:map wh-keymap
              ("C-z" . zoom-window-zoom)))

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

(use-package hl-line
  :ensure t
  :config (progn
          (global-hl-line-mode t)))

(use-package beacon
  :ensure t
  :config (progn
            (require 'beacon)
            (beacon-mode +1)))

(provide 'dot)
;;; dot.el ends here
