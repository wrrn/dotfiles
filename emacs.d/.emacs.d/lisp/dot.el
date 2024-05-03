;;; dot.el --- load all of my custom packages
;;; Commentary:
;;; Code:
(defvar bootstrap-version)
(defvar straight-use-package-by-default t)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(require 'use-package)

(require 'no-littering)
(require 'gcloud)
(require 'org-config)

(use-package autothemer
  :ensure t)

;; (use-package minimal-theme
;;     :ensure t
;;     :config (load-theme 'minimal-light t))

;; (use-package almost-mono-themes
;;   :config
;;   ;; (load-theme 'almost-mono-black t)
;;   ;; (load-theme 'almost-mono-gray t)
;;   (load-theme 'almost-mono-cream t)
;;   ;; (load-theme 'almost-mono-white t)
;;   )

;; (use-package catppuccin-theme
;;   :init
;;   (load-theme 'catppuccin :no-confirm)
;;   (setq catppuccin-flavor 'mocha) ;; or 'frappe, 'latte, 'macchiato, or 'mocha
;;   (catppuccin-reload)
;;   )

(use-package kaolin-themes
  :ensure t
  :config
  (load-theme 'kaolin-valley-light :no-confirm)
  (kaolin-treemacs-theme))

;; (use-package sketch-themes
;;   :config
;;   ;; Load black version
;;   ;; (load-theme 'sketch-black t)
;;   ;; Load white version
;;   (load-theme 'sketch-white t))

;; (use-package subatomic-theme
  ;; :ensure t
  ;; :config (load-theme 'subatomic))

;; (use-package rose-pine-emacs
;;   :ensure t
;;   :straight (rose-pine-emacs :type git :host github :repo "thongpv87/rose-pine-emacs")
;;   :requires autothemer
;;   :config (load-theme 'rose-pine-moon))
;; (load-theme 'rose-pine-moon)

;; (use-package darktooth-theme
;;   :ensure t
;;   :config (load-theme 'darktooth))

;; (use-package soothe-theme
;;   :ensure t
;;   :config (load-theme 'soothe))

;; (use-package jazz-theme
;;   :ensure t
;;   :config (load-theme 'jazz))

;; (use-package flatland-theme
    ;; :ensure t)
;; (use-package goose-theme
  ;; :ensure t)
;; (use-package nano-theme
;;   :ensure t
;;   :config (load-theme 'nano-light))

;; (use-package kanagawa-theme
;;   :straight (kanagawa-theme :type git :host github :repo  "jasonm23/emacs-theme-kanagawa")
;;   :requires autothemer
;;   :config (load-theme 'kanagawa))

;; (use-package nano-modeline
;;   :ensure t
;;   :hook
;;   (prog-mode            . nano-modeline-prog-mode)
;;   (text-mode            . nano-modeline-text-mode)
;;   (org-mode             . nano-modeline-org-mode)
;;   (pdf-view-mode        . nano-modeline-pdf-mode)
;;   (mu4e-headers-mode    . nano-modeline-mu4e-headers-mode)
;;   (mu4e-view-mode       . nano-modeline-mu4e-message-mode)
;;   (mu4e-compose-mode    . nano-modeline-mu4e-compose-mode)
;;   (elfeed-show-mode     . nano-modeline-elfeed-entry-mode)
;;   (elfeed-search-mode   . nano-modeline-elfeed-search-mode)
;;   (elpher-mode          . nano-modeline-elpher-mode)
;;   (term-mode            . nano-modeline-term-mode)
;;   (eat-mode             . nano-modeline-eat-mode)
;;   (xwidget-webkit-mode  . nano-modeline-xwidget-mode)
;;   (messages-buffer-mode . nano-modeline-message-mode)
;;   (org-capture-mode     . nano-modeline-org-capture-mode)
;;   (org-agenda-mode      . nano-modeline-org-agenda-mode)
;;   :config (progn (set-face-attribute 'mode-line nil
;;                     :foreground (face-foreground 'nano-face-subtle)
;;                     :background (face-foreground 'nano-face-subtle)
;;                     :inherit nil
;;                     :box nil)
;;                  (set-face-attribute 'mode-line-inactive nil
;;                     :foreground (face-foreground 'nano-face-subtle)
;;                     :background (face-foreground 'nano-face-subtle)
;;                     :inherit nil
;;                     :box nil)))
  


;; (use-package mini-frame
;;   :ensure t
;;   :config (mini-frame-mode)
;;   ;; :custom
  ;; (mini-frame-show-parameters '((top . 10)
  ;;                               (width . 0.7)
  ;;                               (left . 0.5)))
  ;; )

;; (use-package nano-minibuffer
;;   :ensure t
;;   :straight (nano-minibuffer :type git :host github :repo "rougier/nano-minibuffer"
;;                              :build (:not compile))
;;   :custom nano-minibuffer-position 'bottom)

(use-package nano
  :straight (nano :type git :host github :repo "rougier/nano-emacs"
                  :fork (:host github
                               :repo "wrrn/nano-emacs")
                  :build (:not compile))
  :config (progn
            (set-frame-parameter nil 'internal-border-width 15)
            (custom-set-faces
             `(window-divider ((t (:foreground ,(face-attribute 'default :background)))))
             '(window-divider-first-pixel  ((t (:inherit window-divider))))
             '(window-divider-last-pixel  ((t (:inherit window-divider))))
             '(fringe  ((t (:inherit window-divider)))))
            (set-face-attribute 'mode-line nil
                                :foreground (face-foreground 'nano-face-subtle)
                                :background (face-foreground 'nano-face-subtle)
                                :inherit nil
                                :box nil)
            (set-face-attribute 'mode-line-inactive nil
                                :foreground (face-foreground 'nano-face-subtle)
                                :background (face-foreground 'nano-face-subtle)
                                :inherit nil
                                :box nil)))

(use-package exec-path-from-shell
  ;; Used to get environment variables for mac
  :ensure t
  :init (progn
          (setq-default shell-file-name (getenv "SHELL"))
          (setq-default exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "HOME"))
          (setq-default default-directory (getenv "HOME"))
          (exec-path-from-shell-initialize)))

(use-package asdf
  :ensure t
  :straight (asdf :type git :host github :repo "tabfugnic/asdf.el")
  :custom
  (asdf-binary "/usr/local/opt/asdf/libexec/bin/asdf")
  :config
  (asdf-enable))

(use-package diminish
  :ensure t)

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
                                               :items ,(lambda()
                                                         (when (boundp 'multi-vterm-buffer-name)
                                                         (mapcar #'buffer-name
                                                                 (persp-buffer-list-filter
                                                                  (match-buffers multi-vterm-buffer-name))))))
               'append))
  
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
         ("C-x p t"   . multi-vterm-project)
         :map vterm-mode-map
              ("C-c r" . multi-vterm-rename-buffer)
         :map wh-keymap
               ("t t" . multi-vterm-dedicated-toggle)
               ("t c" . multi-vterm)
               ("t n" . multi-vterm-next)
               ("t p" . multi-vterm-prev)
               ("p t" . multi-vterm-project)
        ))

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
              ("c f"   . eglot-code-actions)
              ("t d"   . display-local-help)
              ("d f"   . xref-find-definitions)
              ("i f"   . eglot-find-implementation)
              ("v r"   . eglot-rename)
              ("4 d f" . xref-find-definitions-other-window)
              ("s f"   . xref-find-apropos)))

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  ;; Enable tree sitter highlighting
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

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
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . ace-window)
         (:map wh-keymap
               ("w j" . ace-window))))

(use-package zoom-window
  :ensure t
  :bind (:map wh-keymap
              ("w z" . zoom-window-zoom)))

;; Show emojis in emacs
;; (use-package emojify
;;   :ensure t
;;   :hook (after-init . global-emojify-mode))

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
              ("c c" . compile)))

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
(require 'writeroom-config)

(provide 'dot)
;; ;;; dot.el ends here
