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

(require 'gcloud)

(use-package minimal-theme
    :ensure t
    :config (load-theme 'minimal-light t))
(use-package darktooth-theme
    :ensure t)
(use-package flatland-theme
    :ensure t)
(use-package goose-theme
  :ensure t)
(use-package nano-theme
    :ensure t)

(use-package nano-modeline
  :ensure t
  :config (nano-modeline-mode))

(use-package mini-frame
  :ensure t
  :config (mini-frame-mode))

(use-package nano-minibuffer
  :straight (nano-minibuffer :type git :host github :repo "rougier/nano-minibuffer"
                             :build (:not compile)))

(use-package nano
  :straight (nano :type git :host github :repo "rougier/nano-emacs"
                  :fork (:host github
                               :repo "wrrn/nano-emacs")
                  :build (:not compile))
  :config (custom-set-faces
           `(window-divider ((t (:foreground ,(face-attribute 'default :background)))))
           '(window-divider-first-pixel  ((t (:inherit window-divider))))
           '(window-divider-last-pixel  ((t (:inherit window-divider))))
           '(fringe  ((t (:inherit window-divider))))))

(use-package exec-path-from-shell
  ;; Used to get environment variables for mac
  :ensure t
  :init (progn
          (setq-default shell-file-name (getenv "SHELL"))
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
        '((sequence "TODO(t)" "TO TEST(e)" "TO DEPLOY(i)" "IN PROGRESS(p)" "In Peer Review(r)" "Waiting(w)" "HOLD(h)" "|" "DONE(d)")
          (sequence "QUESTION(q)" "|" "ANSWERED(a)")
          (sequence "|" "NOT DOING(n)")))
  (org-todo-keyword-faces
        '(("Waiting" . org-warning)
          ("HOLD" . org-warning)
          ("In Peer Review" . org-warning)
          ("TO DEPLOY" . org-warning)
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
  :bind (:map org-mode-map
              ("M-p" . org-metaup)
              ("M-n" . org-metadown))

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

(use-package org-tree-slide
  :ensure t
  :custom
  (org-image-actual-width nil))

(use-package ob-mermaid
  :ensure t
  :custom
  (ob-mermaid-cli-path (concat (getenv "HOME") "/node_modules/.bin/mmdc"))
  :init
  (append org-babel-load-languages '((go . t))))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (concat (getenv "HOME") "/.roam"))
  (org-roam-completion-system 'ivy)
  (org-roam-v2-ack t)
  
  :bind  (("C-c n f" . org-roam-node-find)
          ("C-c n b" . org-roam-switch-to-buffer)
          ("C-c n g" . org-roam-graph)
  :map org-mode-map
  (("C-c n i" . org-roam-node-insert)
   ;; TODO: Use the variable here
   ("C-j" . nil))))

;; Deft helps me look up org-roam files quickly. Like rg or grep but on the fly.
(use-package deft
  :straight (deft :type git :host github :repo "jrblevin/deft"
                  :fork (:host github
                               :repo "wrrn/deft")
                  :build (:not compile))
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
  (org-journal-carryover-items "/!")
  (org-journal-hide-entries-p nil))


(use-package vertico
  :init
  (vertico-mode))


(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package perspective
  :ensure t
  :bind (("C-x k" . persp-kill-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-x M-p"))
  :init
  (persp-mode))

(use-package consult
  :ensure t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
          ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; Isearch integration
         ("C-s" . consult-line)
         ("M-s l m" . consult-line-multi)

         :map wh-keymap
         ("f f" . consult-imenu)
         ("s s" . consult-ripgrep))
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Set the root of the project so that consult-ripgrep starts searches there.
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))

  :config
  (consult-customize consult--source-buffer :hidden t :default nil)
  (consult-customize consult--source-recent-file :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
  (add-to-list 'consult-buffer-sources `(:name "vterminal"
                                               :hidden f
                                               :narrow ?v
                                               :category buffer
                                               :state    ,#'consult--buffer-state
                                               :items ,(lambda() (when (boundp 'multi-vterm-buffer-list) (mapcar #'buffer-name multi-vterm-buffer-list)))) 'append))
  
;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; (use-package embark
;;   :ensure t
;;   :bind
;;   (("C-." . embark-act)         ;; pick some comfortable binding
;;    ("C-;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  ;; :init
  ;; ;; Optionally replace the key help with a completing-read interface
  ;; (setq prefix-help-command #'embark-prefix-help-command)

  ;; :config

  ;; ;; Hide the mode line of the Embark live/completions buffers
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
  ;;                nil
  ;;                (window-parameters (mode-line-format . none)))))

;; ;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :ensure t
;;   :after (embark consult)
;;   :demand t ; only necessary if you have the hook below
;;   ;; if you want to have consult previews as you move around an
;;   ;; auto-updating embark collect buffer
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))

(use-package savehist
  :init (savehist-mode))


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
  :bind (:map wh-keymap
         ("c j" . avy-goto-char)
         ("w j" . avy-goto-word-0 )
         ("l c" . avy-copy-line)
         ("r c" . avy-copy-region)
         ("l k" . avy-kill-whole-line)
         ("r k" . avy-kill-region)))
  


(use-package vterm
  ;; :config
  ;; ;; Remove C-j from the vterm-keymap so that it doesn't conflict with wh-keymap.
  ;; (customize-set-variable 'vterm-keymap-exceptions (add-to-list 'vterm-keymap-exceptions wh-keymap-prefix-key))
  :bind (:map vterm-mode-map
             ("C-j" . nil))
  :ensure t)

(use-package multi-vterm
  :ensure t
  :requires vterm
  :bind (;; ("C-c C-c" . vterm-send-C-c)
         :map vterm-mode-map
              ("C-c r" . multi-vterm-rename-buffer)
         :map wh-keymap
               ("t t" . multi-vterm-dedicated-toggle)
               ("t c" . multi-vterm)
               ("t n" . multi-vterm-next)
               ("t p" . multi-vterm-prev)
               ("p t" . multi-vterm-project)
        ))

(use-package tramp
  :ensure t
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
              ("g s" . magit)
              ("g d" . magit-diff-range)))

(use-package forge
  :after magit
  :config (progn
            (add-to-list 'forge-alist
                         '("ghe.spotify.net"
                           "ghe.spotify.net/api/v3"
                           "ghe.spotify.net"
                           forge-github-repository))
            ))
  
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

            (add-hook 'go-mode-hook #'eglot-ensure)

            ))

(use-package go-rename
  :ensure t)

(use-package go-dlv
  :ensure t)

;; completion-at-point improvements
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Language server
(use-package eglot
  :ensure t
  :bind (
         :map wh-keymap
         ("r f"   . xref-find-references)
         ("t d"   . lsp-describe-thing-at-point)
         ("d f"   . xref-find-definitions)
         ("i f"   . eglot-find-implementation)
         ("4 d f" . xref-find-definitions-other-window)))

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
              ("d c" . desktop-clear)
              ("d s" . desktop-save)
              ("d r" . desktop-read)
              ("d d" . desktop-remove)))

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
           (setq-default rg-command-line-flags '("--sort path")))
  :bind (:map wh-keymap
              ;; Search
              ("s d" . rg-dwim)))
  
(use-package ace-window
  :ensure t
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . ace-window)
         (:map wh-keymap
               ("w j" . ace-window))))

(use-package zoom-window
  :ensure t
  :bind (:map wh-keymap
              ("w z" . zoom-window-zoom)))

;; Show emojis in emacs
(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode))

(use-package linum-off
  :ensure t
  :init (defvar linum-disabled-modes-list '(eshell-mode term-mode ivy-mode compilation-mode org-mode text-mode dired-mode pdf-view-mode)))

 (use-package flyspell
   :ensure t
   :bind (:map flyspell-mode-map
               ("C-." . nil)
          :map wh-keymap
          ("w c" . flyspell-auto-correct-word))
   :hook ((text-mode . flyspell-mode)
          (prog-mode . flyspell-prog-mode)))

(use-package yasnippet
  :ensure t
  :init
  (require 'yasnippet)
  (yas-global-mode 1)
)


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
              ("C-c C-c" . compile)))

(use-package tab-bar
  :bind ("C-x t s" . tab-bar-select-tab-by-name))

(use-package rainbow-delimiters
  :ensure t)

(use-package code-review
  :straight t
  :custom
  (code-review-github-host "ghe.spotify.net/api/v3")
  (code-review-github-graphql-host "ghe.spotify.net/api")
  (code-review-github-base-url "ghe.spotify.net")
  (code-review-log-raw-request-responses t)
  (code-review-github-diffheader '(("Accept" . "application/vnd.github.v3.diff")))
  :bind (:map forge-topic-mode-map
              ("C-c r" . code-review-forge-pr-at-point))
  )

(use-package git-link
  :ensure t
  :config (progn
          (add-to-list 'git-link-remote-alist '("ghe.spotify.net" git-link-github)))
  :bind ("C-c g l" . git-link))

(use-package project
  :ensure t
  :bind (:map wh-keymap
              ("p f" . project-find-file)
))

(require 'meow)
(require 'comments)
(require 'typescript)
(require 'java-development)
(require 'scala-development)

(provide 'dot)
;;; dot.el ends here
